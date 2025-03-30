open Zamlbie

let () =
  let run =
    let open Lwt.Infix in
    match Client_arg.parse_args () with
    | Ok (Client_arg.Join id) -> Client.join_game (Notty_lwt.Term.create ()) id
    | Ok (Client_arg.Test config) -> Client.offline_game (Notty_lwt.Term.create ()) config
    | Ok (Client_arg.Create config) ->
      Client.create_game config
      >>= (function
       | Ok game -> Client.join_game (Notty_lwt.Term.create ()) game.game_id
       | Error err -> failwith (Rest_client.show_error err))
    | Error _ -> failwith "Invalid user input!"
  in
  Lwt_main.run run
;;
