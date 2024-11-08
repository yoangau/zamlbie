open Zamlbie

let () =
  let run =
    let open Lwt.Infix in
    Rest_client.create_game Game.default_config
    >>= function
    | None -> failwith "Game creation failed!"
    | Some game -> Client.join_game game.game_id
  in
  Lwt_main.run run
;;
