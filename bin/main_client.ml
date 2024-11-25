open Zamlbie

let () =
  let run =
    let open Lwt.Infix in
    let open Client_arg in
    match parse_args () with
    | Ok (Join id) -> Client.join_game (Notty_lwt.Term.create ()) id
    | Ok (Create config) ->
      Client.create_game config
      >>= (function
       | None -> failwith "Game creation failed!"
       | Some game -> Client.join_game (Notty_lwt.Term.create ()) game.game_id)
    | Ok (Test config) -> Client.offline_game (Notty_lwt.Term.create ()) config
    | Error _ -> failwith "Invalid user input!"
  in
  Lwt_main.run run
;;
