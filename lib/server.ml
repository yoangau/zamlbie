open Base
open Lwt.Infix

let send message socket =
  let serialized = Message.Serializer.string_of_server_message message in
  Dream.log "Sending message: %s" serialized;
  Dream.send socket serialized
;;

let game_update_message game player_id =
  let player = Game.find_entity_exn game player_id in
  let partitioned_entities = Game.partition_map player game in
  `Update
    (Game.WireFormat.serialize
       ~game_id:game.game_id
       ~config:game.config
       ~entities:partitioned_entities)
;;

let receive socket =
  Dream.receive socket
  >>= function
  | Some message ->
    Dream.log "Raw received message: %s" message;
    message |> Message.Serializer.client_message_of_string |> Lwt_result.return
  | None -> Lwt_result.fail "No message received"
;;

let close_ws = Match.players_ws_iter ~f:(fun ws _ -> Dream.close_websocket ws |> ignore)
let broadcast message = Match.players_ws_iter ~f:(fun ws _ -> send message ws |> ignore)

let send_game_updates game =
  Match.players_ws_iter ~f:(fun ws player_id ->
    send (game_update_message game player_id) ws |> ignore)
;;

let match_orchestrator match_id =
  let execute_player_moves game_match =
    let walls =
      Game.gather_positions
        ~p:(fun e ->
          let open Stdlib in
          e = `Environment `Wall)
        ~entities:game_match.Match.state.entities
    in
    Hashtbl.iteri
      game_match.Match.players
      ~f:
        Stdlib.(
          fun ~key:entity_id ~data:{ mailbox; _ } ->
            mailbox
            |> Option.map (fun move ->
              Game.move ~walls ~game:game_match.state ~entity_id ~move)
            |> Option.join
            |> Option.iter (Match.update_game_state game_match))
  in
  let empty_mailboxes players =
    Hashtbl.iter players ~f:(fun coms -> coms.Match.mailbox <- None)
  in
  let apply_in_game_effects game_match =
    Game.Effects.(apply InGame.effects game_match.state)
    |> Match.update_game_state game_match
  in
  let game_match = Match.Registry.find_exn match_id in
  let%lwt () = Lwt_condition.wait game_match.started in
  send_game_updates game_match.state game_match;
  let start_time = Unix.time () in
  let rec tick () =
    let%lwt () = Lwt_unix.sleep game_match.Match.state.config.tick_delta in
    execute_player_moves game_match;
    empty_mailboxes game_match.players;
    apply_in_game_effects game_match;
    send_game_updates game_match.state game_match;
    match Game.verify_end_conditions game_match.state start_time with
    | None -> tick ()
    | Some (Other _) -> assert false (* future other end state? *)
    | Some (Win who) ->
      Match.Registry.remove match_id;
      broadcast (`GameOver who) game_match;
      Lwt.return_unit
  in
  tick ()
;;

let handle_websocket_connection candidate_match_id player_socket =
  match Match.Registry.find candidate_match_id with
  | None -> send (`Rejected "Game not found!") player_socket
  | Some game_match ->
    (match Match.try_join_match game_match player_socket with
     | None -> send (`Rejected "Game full!") player_socket
     | Some player_id ->
       let%lwt () = send (`Joined player_id) player_socket in
       let%lwt () = send (game_update_message game_match.state player_id) player_socket in
       let rec input_loop () =
         receive player_socket
         >>= function
         | Ok (`Move move) ->
           Match.mailbox_move game_match player_id move;
           input_loop ()
         | Error _ | _ -> Lwt.return ()
       in
       input_loop ())
;;

let run () =
  Dream.run ~error_handler:Dream.debug_error_handler ~interface:"0.0.0.0" ~port:7777
  @@ Dream.logger
  @@ Dream.router
       [ (Dream.post "/create_game"
          @@ fun request ->
          let%lwt body = Dream.body request in
          let config = Game.WireFormat.Serializer.config_of_string body in
          let Game.{ game_id; config; entities } =
            Match.Registry.new_match config match_orchestrator
          in
          Dream.respond
            (Game.WireFormat.Serializer.string_of_game
               (Game.WireFormat.serialize
                  ~game_id
                  ~config
                  ~entities:(Hashtbl.data entities))));
         (Dream.get "/join/:id"
          @@ fun request ->
          let id = Dream.param request "id" |> Int.of_string in
          Dream.websocket @@ handle_websocket_connection id)
       ]
;;
