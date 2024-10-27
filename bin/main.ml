open Zamlbie

let game = Game.init 10 10

let () =
  Client.render Client.terminal game;
  Unix.sleep 1
;;
