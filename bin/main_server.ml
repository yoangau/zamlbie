open Zamlbie

let () =
  Server.create_game Game.default_config |> ignore;
  Server.run ()
;;
