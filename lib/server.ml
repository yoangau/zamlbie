open Base

let games = Hashtbl.create (module Int)
let next_id = ref 0

let create_game width height =
  let new_game = Game.init width height in
  Hashtbl.add_exn games ~key:!next_id ~data:new_game;
  Int.incr next_id;
  new_game
;;

let on_player_input ~id ~player ~direction =
  match Hashtbl.find games id with
  | Some game ->
    let new_game = Game.move game player direction in
    Hashtbl.set games ~key:id ~data:new_game
  | None -> ()
;;

let on_game_update ~id = Hashtbl.find_exn games id
