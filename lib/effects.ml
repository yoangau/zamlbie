let apply effects game = List.fold_left (fun game action -> action game) game effects

module Start = struct
  let generate_walls (game : Game.t) =
    let width, height = (game.config.width, game.config.height) in
    let number_of_floor = game.config.number_of_floor in
    let create_wall x y z =
      Game.{ default_entity with x; y; z; entity_type = `Environment `Wall }
    in
    let rec generate_random_walls_per_floor z n acc =
      if n <= 0
      then acc
      else (
        let x = Random.int width in
        let y = Random.int height in
        let orientation = Random.int 2 in
        let length = Random.int 4 + 2 in
        let new_wall =
          (match orientation with
           | 0 ->
             List.init length (fun i ->
               if x + i < width then Some (create_wall (x + i) y z) else None)
           | _ ->
             List.init length (fun i ->
               if y + i < height then Some (create_wall x (y + i) z) else None))
          |> List.filter_map Fun.id
        in
        generate_random_walls_per_floor z (n - 1) (new_wall @ acc))
    in
    let generate_walls_for_all_floors acc z =
      let num_random_walls = game.config.walls_per_floor in
      generate_random_walls_per_floor z num_random_walls acc
    in
    let walls =
      List.init number_of_floor (fun z -> generate_walls_for_all_floors [] z)
      |> List.flatten
    in
    List.fold_left (fun game wall -> Game.add_entity game wall |> snd) game walls
  ;;

  let generate_stairs (game : Game.t) =
    let walls =
      Game.gather_positions ~p:(fun e -> e = `Environment `Wall) ~entities:game.entities
    in
    let valid_stairs (x, y, z) =
      (* no wall on the staircase going up origin*)
      (not @@ Game.Set3d.mem (x, y, z) walls)
      (* no wall on the staircase going down origin*)
      && (not @@ Game.Set3d.mem (x, y + 1, z + 1) walls)
      (* no wall on the staircase going up destination*)
      && (not @@ Game.Set3d.mem (x, y, z + 1) walls)
      (* no wall on the staircase going down destination*)
      && (not @@ Game.Set3d.mem (x, y - 1, z) walls)
    in
    let width, height, max_z =
      (game.config.width, game.config.height, game.config.number_of_floor - 1)
    in
    (* random position excluding the border *)
    let rec find_valid_pos z =
      let x, y = (Random.int width, Random.int (height - 2) + 1) in
      if valid_stairs (x, y, z) then (x, y, z) else find_valid_pos z
    in
    let create stairs (x, y, z) =
      Game.{ default_entity with x; y; z; entity_type = `Environment stairs }
    in
    let create_updown (x, y, z) =
      [ create `StairsUp (x, y, z); create `StairsDown (x, y + 1, z + 1) ]
    in
    let stairs =
      List.init max_z (fun z ->
        List.init game.config.staircases_per_floor (fun _ -> find_valid_pos z))
      |> List.flatten
      |> List.map create_updown
      |> List.flatten
    in
    List.fold_left (fun game wall -> Game.add_entity game wall |> snd) game stairs
  ;;

  let distribute_players (game : Game.t) =
    let environment_cells =
      Game.gather_positions
        ~p:(function
          | `Environment _ -> true
          | _ -> false)
        ~entities:game.entities
    in
    let is_occupied occupied_cells pos =
      List.mem pos occupied_cells || Game.Set3d.mem pos environment_cells
    in
    let width, height = (game.config.width, game.config.height) in
    let rec generate_unique_position occupied_positions =
      let x = Random.int width in
      let y = Random.int height in
      let z = 0 in
      let pos = (x, y, z) in
      if is_occupied occupied_positions pos
      then generate_unique_position occupied_positions
      else pos
    in
    let players = Game.get_players game in
    let all_positions =
      List.fold_left
        (fun acc _ ->
          let unique_pos = generate_unique_position acc in
          unique_pos :: acc)
        []
        players
    in
    let rec assign_positions players positions =
      match (players, positions) with
      | [], _ | _, [] -> []
      | entity :: rest, (x, y, z) :: pos_rest ->
        let updated_entity : Game.entity = { entity with x; y; z } in
        updated_entity :: assign_positions rest pos_rest
    in
    let updated_players = assign_positions players all_positions in
    List.fold_left Game.update_entity game updated_players
  ;;

  let zombie_sortition (game : Game.t) =
    let players = Game.get_players game in
    let player_count = List.length players in
    let random_player_idx = Random.int player_count in
    let player_zombie_to_be = List.nth players random_player_idx in
    Game.update_entity game { player_zombie_to_be with entity_type = `Player `Zombie }
  ;;

  let effects = [ generate_walls; generate_stairs; distribute_players; zombie_sortition ]
end

module Tick = struct
  let stairs_step (game : Game.t) =
    let stairs_up =
      Game.gather_positions
        ~p:(fun e -> e = `Environment `StairsUp)
        ~entities:game.entities
    in
    let stairs_down =
      Game.gather_positions
        ~p:(fun e -> e = `Environment `StairsDown)
        ~entities:game.entities
    in
    let is_on what ({ x; y; z; _ } : Game.entity) = Game.Set3d.mem (x, y, z) what in
    let step (entity : Game.entity) =
      match entity.entity_type with
      | `Player _ when is_on stairs_up entity -> { entity with z = entity.z + 1 }
      | `Player _ when is_on stairs_down entity -> { entity with z = entity.z - 1 }
      | _ -> entity
    in
    Base.Hashtbl.map_inplace game.entities ~f:step;
    game
  ;;

  let infection (game : Game.t) =
    let zombies =
      Game.gather_positions ~p:(fun e -> e = `Player `Zombie) ~entities:game.entities
    in
    let has_zombie_on_same_cell ({ x; y; z; _ } : Game.entity) =
      Game.Set3d.mem (x, y, z) zombies
    in
    let infect (entity : Game.entity) =
      match entity.entity_type with
      | `Player `Human when has_zombie_on_same_cell entity ->
        { entity with entity_type = `Player `Zombie }
      | _ -> entity
    in
    Base.Hashtbl.map_inplace game.entities ~f:infect;
    game
  ;;

  let effects = [ stairs_step; infection ]
end
