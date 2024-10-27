open Base

let games = Hashtbl.create (module Int)
let next_id = ref 0

let get_agent_move Game.{ entities; _ } =
  let Game.{ x = px; y = py; _ } = List.nth_exn entities 0 in
  let Game.{ x = ax; y = ay; _ } = List.nth_exn entities 1 in
  let x = px - ax in
  let y = py - ay in
  if (x * x) + (y * y) > Game.view_radius_sq
  then (
    match Random.int 4 with
    | 0 -> `Up
    | 1 -> `Down
    | 2 -> `Left
    | 3 -> `Right
    | _ -> assert false)
  else if Int.abs x > Int.abs y
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

let on_player_input ~game_id ~player_id move =
  let ( let* ) = Option.( >>= ) in
  let* game = Hashtbl.find games game_id in
  let agent_move = get_agent_move game in
  let* game = Game.move ~game ~id:player_id ~entity_type:`Player ~move in
  let* game = Game.move ~game ~id:1 ~entity_type:`Player ~move:agent_move in
  Hashtbl.set games ~key:game_id ~data:game;
  Some ()
;;

let on_game_update ~game_id = Hashtbl.find_exn games game_id
