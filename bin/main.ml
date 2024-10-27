open Zamlbie

let game = Game.init 10 10
let () = Client.main_loop Client.terminal game
