module WireFormat = struct
  include Game_t
  module Serializer = Game_j

  let wire_format ~game_id ~entities = { game_id; entities }
end

let () = Random.self_init ()
let entity_id_gen = Uuid.create_gen ()

type config = WireFormat.config

(* Server-side entity with absolute coordinates for game logic *)
type server_entity =
  { entity_type : WireFormat.entity_type;
    id : int;
    x : int;
    y : int;
    z : int
  }

type entity = server_entity

let default_entity : entity =
  { id = 0; entity_type = `Player `Human; x = 0; y = 0; z = 0 }
;;

type t =
  { game_id : int;
    entities : (Uuid.HashtblKey.t, entity) Base.Hashtbl.t;
    config : config
  }

let make game_id config =
  { game_id; entities = Base.Hashtbl.create (module Uuid.HashtblKey); config }
;;

let is_entity a b = WireFormat.(a.id = b.id)
let find_entity { entities; _ } id = Base.Hashtbl.find entities id
let find_entity_exn { entities; _ } id = Base.Hashtbl.find_exn entities id

module Set3d = Set.Make (struct
    type t = int * int * int

    let compare = Stdlib.compare
  end)

let gather_positions ~p ~entities =
  Base.Hashtbl.fold entities ~init:Set3d.empty ~f:(fun ~key:_ ~data positions ->
    let { entity_type; x; y; z; _ } = data in
    if p entity_type then Set3d.add (x, y, z) positions else positions)
;;

let get_players game =
  Base.Hashtbl.data game.entities
  |> List.filter_map (fun e ->
    match e.entity_type with
    | `Player _ -> Some e
    | _ -> None)
;;

module Set2d = Set.Make (struct
    type t = int * int

    let compare = Stdlib.compare
  end)

module Costs = Map.Make (struct
    type t = int * int * int

    let compare = compare
  end)

let propagation_costs entities =
  Base.Hashtbl.fold
    entities
    ~init:Costs.empty
    ~f:(fun ~key:_ ~data:{ x; y; z; entity_type; _ } grid ->
      match entity_type with
      | `Environment `Wall -> Costs.add (x, y, z) 1000 grid
      | `Environment `Glass -> Costs.add (x, y, z) 1 grid
      | _ -> grid)
;;

let raycast_visibility ~pos:(px, py, pz) ~energy ~propagation_costs =
  let num_rays = 32 in
  let angle_step = 2.0 *. Float.pi /. float_of_int num_rays in
  let cast_ray angle =
    let dx, dy = (cos angle, sin angle) in
    let rec step fx fy energy acc =
      let x, y = (int_of_float (fx +. 0.5), int_of_float (fy +. 0.5)) in
      let cost = Costs.find_opt (x, y, pz) propagation_costs |> Option.value ~default:0 in
      let new_acc = (x, y) :: acc in
      if cost >= 1000 || energy - cost <= 0
      then new_acc
      else step (fx +. (dx *. 0.5)) (fy +. (dy *. 0.5)) (energy - cost - 1) new_acc
    in
    step (float_of_int px) (float_of_int py) energy []
  in
  List.init num_rays (fun i -> float_of_int i *. angle_step)
  |> List.fold_left (fun acc angle -> List.rev_append (cast_ray angle) acc) []
  |> List.sort_uniq compare
;;

let visible_map_relative id game =
  let player_entity = find_entity_exn game id in
  let { x = px; y = py; z = pz; _ } = player_entity in
  let energy =
    match player_entity.entity_type with
    | `Player `Human -> game.config.human_view_radius
    | `Player `Zombie -> game.config.zombie_view_radius
    | _ -> game.config.human_view_radius
  in
  let propagation_costs = propagation_costs game.entities in
  let positions_to_send =
    raycast_visibility ~pos:(px, py, pz) ~energy ~propagation_costs |> Set2d.of_list
  in
  let theme = Theme.get_theme_by_index pz in
  (* Create a set of positions that have real entities *)
  let real_entities_positions =
    game.entities
    |> Base.Hashtbl.data
    |> List.filter (fun ({ z; _ } : entity) -> z = pz)
    |> List.fold_left (fun acc ({ x; y; _ } : entity) -> Set2d.add (x, y) acc) Set2d.empty
  in
  let visible_entities =
    game.entities
    |> Base.Hashtbl.data
    |> List.filter (fun ({ x; y; z; _ } : entity) ->
      z = pz && Set2d.mem (x, y) positions_to_send)
    |> List.map (fun ({ entity_type; id; x; y; _ } : entity) ->
      WireFormat.{ entity_type; id; x = x - px; y = y - py; theme })
  in
  (* Generate floor entities and boundary walls for all visible positions *)
  let generated_entities =
    let entities = ref [] in
    let entity_id = ref (-1) in
    (* Counter for virtual entity IDs *)

    (* Generate virtual entity with unique ID *)
    let make_virtual_entity entity_type rx ry =
      decr entity_id;
      WireFormat.
        { entity_type;
          id = !entity_id;
          (* Use negative IDs for virtual entities *)
          x = rx;
          y = ry;
          theme
        }
    in
    (* Check each position in the visible area *)
    Set2d.iter
      (fun (gx, gy) ->
        let rx = gx - px in
        let ry = gy - py in
        (* Check if this position is at the immediate map boundary (single edge layer) *)
        let is_boundary_edge =
          (gx = -1 && gy >= 0 && gy < game.config.height)
          (* Left edge *)
          || (gx = game.config.width && gy >= 0 && gy < game.config.height)
          (* Right edge *)
          || (gy = -1 && gx >= 0 && gx < game.config.width)
          (* Top edge *)
          || (gy = game.config.height && gx >= 0 && gx < game.config.width)
          (* Bottom edge *)
          || (gx = -1 && gy = -1)
          (* Top-left corner *)
          || (gx = -1 && gy = game.config.height)
          (* Bottom-left corner *)
          || (gx = game.config.width && gy = -1)
          || (* Top-right corner *)
          (gx = game.config.width && gy = game.config.height)
          (* Bottom-right corner *)
        in
        if is_boundary_edge
        then
          (* Generate boundary wall only at immediate edge *)
          entities := make_virtual_entity (`Environment `Wall) rx ry :: !entities
        else if (gx >= 0 && gx < game.config.width && gy >= 0 && gy < game.config.height)
                && not (Set2d.mem (gx, gy) real_entities_positions)
        then
          (* Generate floor for empty visible positions inside the map only *)
          entities := make_virtual_entity (`Environment `Floor) rx ry :: !entities)
      positions_to_send;
    !entities
  in
  visible_entities @ generated_entities
;;

let get_move_delta = function
  | `Up -> (0, -1)
  | `Down -> (0, 1)
  | `Left -> (-1, 0)
  | `Right -> (1, 0)
;;

let add_entity game entity =
  let id = Uuid.next_id entity_id_gen in
  Base.Hashtbl.set game.entities ~key:id ~data:{ entity with id };
  (id, game)
;;

let remove_entity game entity_id =
  Base.Hashtbl.remove game.entities entity_id;
  game
;;

let update_entity game entity =
  Base.Hashtbl.set game.entities ~key:entity.id ~data:entity;
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
  if Set3d.mem (nx, ny, entity.z) walls
  then None
  else Some (update_entity game { entity with x = nx; y = ny })
;;

type game_ended =
  | Win of WireFormat.character_type
  | Other of string

let verify_end_conditions game start_time =
  let all_zombie entities =
    not
    @@ Base.Hashtbl.exists entities ~f:(fun e ->
      match e.entity_type with
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
