open Base
open Lwt.Infix

let send message socket =
  let serialized = Message.Serializer.string_of_server_message message in
  Dream.log "Sending message %s" serialized;
  Dream.send socket serialized
;;

let receive socket =
  Dream.receive socket
  >>= function
  | Some message ->
    Dream.log "Raw received message: %S" message;
    message |> Message.Serializer.client_message_of_string |> Lwt_result.return
  | None -> Lwt_result.fail "No message received"
;;

let broadcast players message =
  players
  |> Stdlib.Array.to_list
  |> Stdlib.List.filter_map (fun (_, w) -> w)
  |> Lwt_list.iter_p (send message)
;;

let game_orchestrator match_id =
  let execute_player_moves game_match =
    Stdlib.Array.iteri
      Stdlib.(
        fun idx (maybe_move, _) ->
          maybe_move
          |> Option.map (fun move -> Game.move ~game:game_match.state ~id:idx ~move)
          |> Option.join
          |> Option.iter (Match.update_game_state game_match))
      game_match.players
  in
  let empty_mailboxes = Stdlib.Array.map_inplace (fun (_, ws) -> (None, ws)) in
  let game_match = Match.Registry.find_exn match_id in
  let rec tick () =
    let%lwt () = Lwt_unix.sleep game_match.Match.state.config.tick_delta in
    execute_player_moves game_match;
    empty_mailboxes game_match.players;
    let%lwt () = broadcast game_match.players (`Update game_match.state) in
    tick ()
  in
  tick ()
;;

let handle_websocket_connection candidate_match_id player_socket =
  match Match.Registry.find candidate_match_id with
  | None -> send (`Rejected "Game not found!") player_socket
  | Some game_match ->
    (match Match.try_join_match game_match player_socket with
     | None -> send (`Rejected "Game full!") player_socket
     | Some client_id ->
       let%lwt () = send (`Joined client_id) player_socket in
       let%lwt () = send (`Update game_match.state) player_socket in
       let rec input_loop () =
         receive player_socket
         >>= function
         | Ok (`Move move) ->
           Match.mailbox_move game_match client_id move;
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
          let config = Game.Serializer.config_of_string body in
          let game_match = Match.Registry.new_match config game_orchestrator in
          Dream.respond (Game.Serializer.string_of_game game_match));
         (Dream.get "/join/:id"
          @@ fun request ->
          let id = Dream.param request "id" |> Int.of_string in
          Dream.websocket @@ handle_websocket_connection id)
       ]
;;
