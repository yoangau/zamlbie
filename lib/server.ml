open Base
open Lwt.Infix

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
  let ( let* ) = Option.( >>= ) in
  (* let* game = Hashtbl.find games game_id in *)
  let agent_move = get_agent_move game in
  let* game = Game.move ~game ~id:player_id ~entity_type:`Player ~move in
  let* game = Game.move ~game ~id:1 ~entity_type:`Player ~move:agent_move in
  (* Hashtbl.set games ~key:game_id ~data:game; *)
  Some game
;;

let on_game_update ~game_id = Hashtbl.find_exn games game_id

let join_match game_match player_socket =
  if List.length game_match.players < 2
  then game_match.players <- player_socket :: game_match.players;
  if List.length game_match.players = 2 then game_match.game_started <- true;
  Lwt.return game_match.game_started
;;

(* Function to broadcast game start signal to players *)
let broadcast_start_signal game_match =
  Lwt_list.iter_p (fun player -> Dream.send player "Game Start Signal") game_match.players
;;

(* Function to handle game moves and broadcast updated state *)
let update_and_broadcast_state game_match =
  (* Update game state (this can be extended with actual game logic) *)
  Lwt_list.iter_p
    (fun player -> Dream.send player "Game State Updated")
    game_match.players
;;

(* Handle each client WebSocket connection *)
let handle_websocket_connection game_match player_socket =
  let rec loop game_match () =
    Dream.receive player_socket
    >>= function
    | Some message ->
      Dream.log "%s" message;
      (match message with
       | "Hello" -> Dream.send player_socket "Hello, World!"
       | "join_match" ->
         (* Add player to match and check if the game can start *)
         join_match game_match player_socket
         >>= fun game_ready ->
         if game_ready
         then broadcast_start_signal game_match
         else Dream.send player_socket "Join Successful"
       | _move when game_match.game_started ->
         (* Handle a game move if the game has started *)
         update_and_broadcast_state game_match
       | _ -> Dream.send player_socket "Invalid command")
      >>= loop game_match
    | None ->
      (* Remove player from match on disconnect *)
      game_match.players
      <- List.filter ~f:(fun p -> not (phys_equal p player_socket)) game_match.players;
      Lwt.return_unit
  in
  loop game_match ()
;;

(* Set up WebSocket routes *)
let run () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router
       [ (* [ Dream.get "/ws" (fun _request -> Dream.websocket handle_websocket_connection); *)
         (Dream.get "/join/:id"
          @@ fun request ->
          let id = Dream.param request "id" |> Int.of_string in
          match Hashtbl.find games id with
          | Some game_match -> Dream.websocket @@ handle_websocket_connection game_match
          | None -> Dream.respond "Game not found")
       ]
;;
