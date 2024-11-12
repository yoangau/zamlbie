module WireFormat = struct
  include Game_t
  module Serializer = Game_j

  let serialize ~game_id ~config ~entities = { game_id; config; entities }
end

let () = Random.self_init ()
let game_id_gen = Uuid.create_gen ()

type config = WireFormat.config
type entity = WireFormat.entity

type t =
  { game_id : int;
    entities : (Base.Int.t, entity) Base.Hashtbl.t;
    config : config
  }

module Set = Set.Make (struct
    type t = int * int

    let compare = Stdlib.compare
  end)

let gather_positions ~p ~entities =
  Base.Hashtbl.fold
    entities
    ~init:Set.empty
    ~f:(fun ~key:_ ~data:WireFormat.{ entity_type; x; y; _ } positions ->
      if p entity_type then Set.add (x, y) positions else positions)
;;

let partition_map ({ x = px; y = py; _ } : entity) { entities; _ } =
  let positions_to_send =
    let w, h = (20, 20) in
    List.init w (fun x -> List.init h (fun y -> (px + x - (w / 2), py + y - (h / 2))))
    |> List.flatten
    |> Set.of_list
  in
  entities
  |> Base.Hashtbl.data
  |> List.filter (fun ({ x; y; _ } : entity) -> Set.mem (x, y) positions_to_send)
;;

let make game_id config =
  { game_id; entities = Base.Hashtbl.create (module Uuid.HashtblKey); config }
;;

let get_move_delta = function
  | `Up -> (0, -1)
  | `Down -> (0, 1)
  | `Left -> (-1, 0)
  | `Right -> (1, 0)
;;

let default_entity = WireFormat.{ id = 0; entity_type = `Player `Human; x = 0; y = 0 }
let is_entity a b = WireFormat.(a.id = b.id)
let find_entity { entities; _ } id = Base.Hashtbl.find entities id
let find_entity_exn { entities; _ } id = Base.Hashtbl.find_exn entities id

let add_entity game entity =
  let id = Uuid.next_id game_id_gen in
  Base.Hashtbl.set game.entities ~key:id ~data:{ entity with id };
  (id, game)
;;

let update_entity game new_entity =
  Base.Hashtbl.set game.entities ~key:new_entity.WireFormat.id ~data:new_entity;
  game
;;

let move ~walls ~game ~entity_id ~move =
  let ( let* ) = Base.Option.( >>= ) in
  let* entity = find_entity game entity_id in
  let dx, dy = get_move_delta move in
  let nx, ny =
    ( Base.Int.clamp_exn (entity.x + dx) ~min:0 ~max:(game.config.width - 1),
      Base.Int.clamp_exn (entity.y + dy) ~min:0 ~max:(game.config.height - 1) )
  in
  if Set.mem (nx, ny) walls
  then None
  else Some (update_entity game { entity with x = nx; y = ny })
;;

let get_players game =
  Base.Hashtbl.data game.entities
  |> List.filter_map (fun e ->
    match e.WireFormat.entity_type with
    | `Player _ -> Some e
    | _ -> None)
;;

module Effects = struct
  let apply effects game = List.fold_left (fun game action -> action game) game effects

  module Start = struct
    let zombie_sortition game =
      let players = get_players game in
      let player_count = List.length players in
      let random_player_idx = Random.int player_count in
      let player_zombie_to_be = List.nth players random_player_idx in
      update_entity game { player_zombie_to_be with entity_type = `Player `Zombie }
    ;;

    let distribute_players game =
      let width, height = (game.config.width, game.config.height) in
      let rec generate_unique_position occupied_positions =
        let x = Random.int width in
        let y = Random.int height in
        let pos = (x, y) in
        if List.mem pos occupied_positions
        then generate_unique_position occupied_positions
        else pos
      in
      let all_positions =
        Base.Hashtbl.fold game.entities ~init:[] ~f:(fun ~key:_ ~data:_ acc ->
          let unique_pos = generate_unique_position acc in
          unique_pos :: acc)
      in
      let rec assign_positions players positions =
        match (players, positions) with
        | [], _ | _, [] -> []
        | entity :: rest, (x, y) :: pos_rest ->
          let updated_entity = WireFormat.{ entity with x; y } in
          updated_entity :: assign_positions rest pos_rest
      in
      let players = get_players game in
      let updated_players = assign_positions players all_positions in
      List.fold_left update_entity game updated_players
    ;;

    let generate_walls game =
      let occupied_positions =
        Base.Hashtbl.data game.entities |> List.map (fun e -> (e.WireFormat.x, e.y))
      in
      let is_already_occupied xy = List.mem xy occupied_positions in
      let width, height = (game.config.width, game.config.height) in
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
            (match orientation with
             | 0 ->
               List.init length (fun i ->
                 if x + i < width && not (is_already_occupied (x + i, y))
                 then Some (create_wall (x + i) y)
                 else None)
             | _ ->
               List.init length (fun i ->
                 if y + i < height && not (is_already_occupied (x, y + i))
                 then Some (create_wall x (y + i))
                 else None))
            |> List.filter_map Fun.id
          in
          generate_random_walls (n - 1) (new_wall @ acc))
      in
      let num_random_walls = 10 in
      let walls = generate_random_walls num_random_walls [] in
      List.fold_left (fun game wall -> add_entity game wall |> snd) game walls
    ;;

    let effects = [ zombie_sortition; distribute_players; generate_walls ]
  end

  module InGame = struct
    let infection game =
      let zombie_positions =
        gather_positions ~p:(fun e -> e = `Player `Zombie) ~entities:game.entities
      in
      let has_zombie_on_same_cell WireFormat.{ x; y; _ } =
        Set.mem (x, y) zombie_positions
      in
      let infect entity =
        match entity.WireFormat.entity_type with
        | `Player `Human when has_zombie_on_same_cell entity ->
          { entity with entity_type = `Player `Zombie }
        | _ -> entity
      in
      Base.Hashtbl.map_inplace game.entities ~f:infect;
      game
    ;;

    let effects = [ infection ]
  end
end

type game_ended =
  | Win of WireFormat.character_type
  | Other of string

let verify_end_conditions game start_time =
  let all_zombie entities =
    not
    @@ Base.Hashtbl.exists entities ~f:(fun e ->
      match e.WireFormat.entity_type with
      | `Player `Human -> true
      | _ -> false)
  in
  let now = Unix.time () in
  if int_of_float (now -. start_time) >= game.config.time_limit
  then Some (Win `Human)
  else if all_zombie game.entities
  then Some (Win `Zombie)
  else None
;;
