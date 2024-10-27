open Zamlbie

let game = Server.create_game 40 40
let () = Client.main_loop Client.terminal game 0
