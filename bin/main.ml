open Zamlbie

let game = Server.create_game 40 20
let () = Client.main_loop Client.terminal game 0
