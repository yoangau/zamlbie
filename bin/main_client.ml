open Zamlbie

let () =
  let run =
    let open Lwt.Infix in
    match Cmd.parse_args () with
    | Ok (Cmd.Join id) -> Client.join_game (Notty_lwt.Term.create ()) id
    | Ok (Cmd.Test config) -> Client.offline_game (Notty_lwt.Term.create ()) config
    | Ok (Cmd.Create config) ->
      Client.create_game config
      >>= (function
       | None -> failwith "Game creation failed!"
       | Some game -> Client.join_game (Notty_lwt.Term.create ()) game.game_id)
    | Error _ -> failwith "Invalid user input!"
  in
  Lwt_main.run run
;;
