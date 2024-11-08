open Base
open Lwt.Infix
open Lwt.Syntax

type match_state =
  { players : (Game.move option * Dream.websocket option) array;
    (* List of player connections *)
    mutable game_started : bool;
    mutable state : Game.game (* Example game state *)
  }

let games = Hashtbl.create (module Int)
let next_id = ref 0
let find_game ~game_id = Hashtbl.find_exn games game_id

let create_game config =
  let new_game = Game.init (Hashtbl.length games) config in
  (* let thread game_id =
     let step () =
     let game = find_game ~game_id in
     let tick () = Lwt_unix.sleep game.config.tick_rate in
     let List.map receive game.players
  *)
  Hashtbl.add_exn
    games
    ~key:!next_id
    ~data:
      { players = Stdlib.Array.make new_game.config.player_count (None, None);
        game_started = true;
        state = new_game
      };
  Int.incr next_id;
  new_game
;;

let on_player_input ~game ~player_id move = Game.move ~game ~id:player_id ~move

let try_join_match game_match player_socket =
  let room_size = game_match.state.config.player_count in
  let previous_player_count =
    Stdlib.Array.fold_left
      (fun acc (_, s) -> acc + if Option.is_some s then 1 else 0)
      0
      game_match.players
  in
  if previous_player_count = room_size
  then None
  else (
    if previous_player_count < room_size
    then (
      game_match.players.(previous_player_count) <- (None, Some player_socket);
      game_match.state
      <- Game.add_entity
           game_match.state
           { Game.default_entity with id = previous_player_count });
    if previous_player_count + 1 = room_size then game_match.game_started <- true;
    Some previous_player_count)
;;

let send message socket =
  Dream.send socket @@ Message.Serializer.string_of_server_message message
;;

let receive socket =
  Dream.receive socket
  >>= function
  | Some message ->
    Dream.log "Raw received message: %S" message;
    message |> Message.Serializer.client_message_of_string |> Lwt_result.return
  | None -> Lwt_result.fail "No message received"
;;

(* Function to broadcast game state to all players *)

let broadcast_game game_match =
  game_match.players
  |> Stdlib.Array.to_list
  |> Stdlib.List.filter_map (fun (_, w) -> w)
  |> Lwt_list.iter_s (send (`Update game_match.state))
;;

(* Handle each client WebSocket connection *)
let handle_websocket_connection maybe_game player_socket =
  match maybe_game with
  | None -> send (`Rejected "Game not found!") player_socket
  | Some game_match ->
    (match try_join_match game_match player_socket with
     | None -> send (`Rejected "Game full!") player_socket
     | Some client_id ->
       let* () = send (`Joined client_id) player_socket in
       let* () = broadcast_game game_match in
       let rec input_loop () =
         receive player_socket
         >>= function
         | Ok (`Move move) ->
           (match on_player_input ~game:game_match.state ~player_id:client_id move with
            | Some new_game ->
              game_match.state <- new_game;
              broadcast_game game_match |> Lwt.ignore_result;
              input_loop ()
            | None -> input_loop ())
         | Error _ | _ -> Lwt.return ()
       in
       input_loop ())
;;

(* Set up WebSocket routes *)
let run () =
  Dream.run ~error_handler:Dream.debug_error_handler ~interface:"0.0.0.0" ~port:7777
  @@ Dream.logger
  @@ Dream.router
       (* TODO: Add parameter for game config width height n_player vision tick_speed*)
       [ (Dream.post "/create_game"
          @@ fun request ->
          let%lwt body = Dream.body request in
          let config = Game.Serializer.config_of_string body in
          let game = create_game config in
          Dream.respond (Game.Serializer.string_of_game game));
         (Dream.get "/join/:id"
          @@ fun request ->
          let id = Dream.param request "id" |> Int.of_string in
          let maybe_game = Hashtbl.find games id in
          Dream.websocket @@ handle_websocket_connection maybe_game)
       ]
;;
