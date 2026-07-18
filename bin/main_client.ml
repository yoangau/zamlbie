open Zamlbie

let () =
  let args = Client_arg.parse_args () in
  let server_url = Config.get_server_url ?url:args.server_url () in
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  match args.command with
  | Client_arg.Join id -> Client.join_game ~env ~sw ~server_url id
  | Client_arg.Test config -> Client.offline_game ~env config
  | Client_arg.List -> Client.list_lobbies ~env ~server_url ()
  | Client_arg.Create config ->
    (match Client.create_game ~env ~server_url config with
     | Ok game -> Client.join_game ~env ~sw ~server_url game.game_id
     | Error err -> failwith (Httpc.Raw_client.show_error err))
;;
