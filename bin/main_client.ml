open Zamlbie

let () =
  let run =
    let open Lwt.Infix in
    match Client_arg.parse_args () with
    | Client_arg.Join id -> Client.join_game (Notty_lwt.Term.create ()) id
    | Client_arg.Test config -> Client.offline_game (Notty_lwt.Term.create ()) config
    | Client_arg.List -> Client.list_lobbies ()
    | Client_arg.Create config ->
      Client.create_game config
      >>= (function
       | Ok game -> Client.join_game (Notty_lwt.Term.create ()) game.game_id
       | Error err -> failwith (Http.Raw_client.show_error err))
  in
  Lwt_main.run run
;;
