open Base

let games = Hashtbl.create (module Int)
let next_id = ref 0

let get_agent_move Game.{ entities; _ } =
  let px, py = List.nth_exn entities 0 in
  let ax, ay = List.nth_exn entities 1 in
  let x = px - ax in
  let y = py - ay in
  if Int.abs x > Int.abs y
  then if x >= 0 then `Right else `Left
  else if y >= 0
  then `Down
  else `Up
;;

let create_game width height =
  let new_game = Game.init width height in
  Hashtbl.add_exn games ~key:!next_id ~data:new_game;
  Int.incr next_id;
  new_game
;;

let on_player_input ~id ~player move =
  match Hashtbl.find games id with
  | Some game ->
    let agent_move = get_agent_move game in
    let game = Game.move game player move in
    let game = Game.move game 1 agent_move in
    Hashtbl.set games ~key:id ~data:game
  | None -> ()
;;

let on_game_update ~id = Hashtbl.find_exn games id
