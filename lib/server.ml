open Base
open Lwt.Infix
open Lwt.Syntax

type match_state =
  { mutable players : Dream.websocket list; (* List of player connections *)
    mutable game_started : bool;
    mutable state : Game_j.game (* Example game state *)
  }

let games = Hashtbl.create (module Int)
let next_id = ref 0

let get_agent_move Game.{ entities; _ } =
  let Game.{ x = px; y = py; _ } = List.nth_exn entities 0 in
  let Game.{ x = ax; y = ay; _ } = List.nth_exn entities 1 in
  let x = px - ax in
  let y = py - ay in
  if (x * x) + (y * y) > Game.view_radius_sq
  then (
    match Random.int 4 with
    | 0 -> `Up
    | 1 -> `Down
    | 2 -> `Left
    | 3 -> `Right
    | _ -> assert false)
  else if Int.abs x > Int.abs y
  then if x >= 0 then `Right else `Left
  else if y >= 0
  then `Down
  else `Up
;;

let create_game width height =
  let new_game = Game.init width height in
  Hashtbl.add_exn
    games
    ~key:!next_id
    ~data:{ players = []; game_started = true; state = new_game };
  Int.incr next_id;
  new_game
;;

let on_player_input ~game ~player_id move =
  Game.move ~game ~id:player_id ~entity_type:`Player ~move
;;

let on_game_update ~game_id = Hashtbl.find_exn games game_id

let join_match game_match player_socket =
  game_match.state
  <- Game.add_entity
       game_match.state
       { Game.default_entity with id = List.length game_match.state.entities };
  if List.length game_match.players < 2
  then game_match.players <- player_socket :: game_match.players;
  if List.length game_match.players = 2 then game_match.game_started <- true;
  Lwt.return game_match.game_started
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
  Lwt_list.iter_s (send (`Update game_match.state)) game_match.players
;;

(* Handle each client WebSocket connection *)
let handle_websocket_connection game_match player_socket =
  let* _game_is_starting = join_match game_match player_socket in
  let* () = broadcast_game game_match in
  let rec game_match_loop () =
    receive player_socket
    >>= function
    | Ok (`Move move) ->
      (match on_player_input ~game:game_match.state ~player_id:0 move with
       | Some new_game ->
         game_match.state <- new_game;
         broadcast_game game_match |> Lwt.ignore_result;
         game_match_loop ()
       | None -> game_match_loop ())
    | Error _ | _ -> Lwt.return ()
  in
  game_match_loop ()
;;

(* Set up WebSocket routes *)
let run () =
  Dream.run ~error_handler:Dream.debug_error_handler ~interface:"0.0.0.0" ~port:8080
  @@ Dream.logger
  @@ Dream.router
       (* TODO: Add parameter for game config width height n_player vision tick_speed*)
       [ (Dream.post "/create_game"
          @@ fun _ ->
          let game = create_game 40 40 in
          Dream.respond (Game.Serializer.string_of_game game));
         (Dream.get "/join/:id"
          @@ fun request ->
          let id = Dream.param request "id" |> Int.of_string in
          match Hashtbl.find games id with
          | Some game_match -> Dream.websocket @@ handle_websocket_connection game_match
          | None -> Dream.respond "Game not found")
       ]
;;
