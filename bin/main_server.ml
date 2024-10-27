open Zamlbie

let () =
  Server.create_game 40 40 |> ignore;
  Server.run ()
;;
