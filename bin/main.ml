open Zamlbie

let game = Server.create_game 10 10
let () = Client.main_loop Client.terminal game 0
