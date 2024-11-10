include Game_t
module Serializer = Game_j

let () = Random.self_init ()

let get_move_delta = function
  | `Up -> (0, -1)
  | `Down -> (0, 1)
  | `Left -> (-1, 0)
  | `Right -> (1, 0)
;;

let default_entity = { id = 0; entity_type = `Player `Human; x = 0; y = 0 }
let is_entity a b = a.id = b.id
let find_entity { entities; _ } id = List.find_opt (fun a -> a.id = id) entities

let add_entity game entity =
  let id = Uuid.next_id () in
  (id, { game with entities = { entity with id } :: game.entities })
;;

let update_entity game new_entity =
  { game with
    entities =
      List.map
        (fun entity -> if is_entity entity new_entity then new_entity else entity)
        game.entities
  }
;;

let move ~game ~id ~move =
  let ( let* ) = Base.Option.( >>= ) in
  let* entity = find_entity game id in
  let dx, dy = get_move_delta move in
  let nx, ny =
    ( Base.Int.clamp_exn (entity.x + dx) ~min:0 ~max:(game.config.width - 1),
      Base.Int.clamp_exn (entity.y + dy) ~min:0 ~max:(game.config.height - 1) )
  in
  Some (update_entity game { entity with x = nx; y = ny })
;;

let apply_start_effects game =
  let zombie_sortition game =
    let player_ids =
      List.filter_map
        (fun e ->
          match e.entity_type with
          | `Player `Human -> Some e
          | _ -> None)
        game.entities
    in
    let player_count = List.length player_ids in
    let random_player_idx = Random.int player_count in
    let player_zombie_to_be = List.nth player_ids random_player_idx in
    update_entity game { player_zombie_to_be with entity_type = `Player `Zombie }
  in
  let distribute_players game =
    let width, height = (game.config.width, game.config.height) in
    let all_positions =
      List.init width (fun x -> List.init height (fun y -> (x, y))) |> List.flatten
    in
    let shuffled_positions = List.sort (fun _ _ -> Random.int 3 - 1) all_positions in
    let rec assign_positions entities positions =
      match (entities, positions) with
      | [], _ | _, [] -> []
      | entity :: rest, pos :: pos_rest ->
        let updated_entity = { entity with x = fst pos; y = snd pos } in
        updated_entity :: assign_positions rest pos_rest
    in
    let players, others =
      (List.partition
         (fun e ->
           match e.entity_type with
           | `Player _ -> true
           | _ -> false)
         game.entities [@warning "-11"])
    in
    let updated_players = assign_positions players shuffled_positions in
    { game with entities = updated_players @ others }
  in
  let actions = [ zombie_sortition; distribute_players ] in
  List.fold_left (fun game action -> action game) game actions
;;

let apply_in_game_effects game =
  let infection game =
    let has_zombie_on_same_cell entity =
      List.exists
        (fun other ->
          other.entity_type = `Player `Zombie && other.x = entity.x && other.y = entity.y)
        game.entities
    in
    let infect entity =
      match entity.entity_type with
      | `Player `Human when has_zombie_on_same_cell entity ->
        { entity with entity_type = `Player `Zombie }
      | _ -> entity
    in
    { game with entities = List.map infect game.entities }
  in
  let actions = [ infection ] in
  List.fold_left (fun game action -> action game) game actions
;;

type game_ended =
  | Win of character_type
  | Other of string

let verify_end_conditions game start_time =
  let all_zombie entities =
    entities
    |> List.for_all (fun e ->
      match e.entity_type with
      | `Player `Human -> false
      | _ -> true)
  in
  let now = Unix.time () in
  if int_of_float (now -. start_time) >= game.config.time_limit
  then Some (Win `Human)
  else if all_zombie game.entities
  then Some (Win `Zombie)
  else None
;;

let make game_id config =
  (* random to begin, sophisticaed algo later *)
  let walls =
    let width, height = (config.width, config.height) in
    let create_wall x y =
      { default_entity with x; y; entity_type = `Environment `Wall }
    in
    let rec generate_random_walls n acc =
      if n <= 0
      then acc
      else (
        let x = Random.int width in
        let y = Random.int height in
        let orientation = Random.int 2 in
        let length = Random.int 4 + 2 in
        let new_wall =
          match orientation with
          | 0 ->
            List.init length (fun i ->
              if x + i < width then create_wall (x + i) y else create_wall x y)
          | _ ->
            List.init length (fun i ->
              if y + i < height then create_wall x (y + i) else create_wall x y)
        in
        generate_random_walls (n - 1) (new_wall @ acc))
    in
    let num_random_walls = 10 in
    generate_random_walls num_random_walls []
  in
  let game = { game_id; entities = []; config } in
  List.fold_left (fun game wall -> add_entity game wall |> snd) game walls
;;
