open Notty_lwt

let send_player_input terminal () =
  Lwt_stream.map_s
    (function
      | `Key (`Arrow move, []) ->
        Lwt.return @@ Some (Message.Serializer.string_of_client_message (`Move move))
      | `Key (`Escape, []) -> Lwt.return None
      | _ -> Lwt.return @@ Some "lol")
    (Term.events terminal)
;;

let receive message =
  print_endline @@ "message received: " ^ message;
  (match Message.Serializer.server_message_of_string message with
   | `Update _updated_game -> print_endline "received update"
   | _ -> print_endline "other");
  Lwt.return ()
;;

let run () =
  let uri = Uri.of_string "http://127.0.0.1:8080/join/0" in
  let send_player_input = send_player_input (Term.create ()) in
  Lwt_main.run (Ws_client.client uri receive send_player_input);
  Unix.sleep 5
;;
