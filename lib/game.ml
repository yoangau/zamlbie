module WireFormat = struct
  include Game_t
  module Serializer = Game_j

  let wire_format ~game_id ~config ~entities = { game_id; config; entities }
end

let () = Random.self_init ()
let entity_id_gen = Uuid.create_gen ()

type config = WireFormat.config
type entity = WireFormat.entity

let default_entity =
  WireFormat.{ id = 0; entity_type = `Player `Human; x = 0; y = 0; z = 0 }
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
  Base.Hashtbl.fold
    entities
    ~init:Set3d.empty
    ~f:(fun ~key:_ ~data:WireFormat.{ entity_type; x; y; z; _ } positions ->
      if p entity_type then Set3d.add (x, y, z) positions else positions)
;;

let get_players game =
  Base.Hashtbl.data game.entities
  |> List.filter_map (fun e ->
    match e.WireFormat.entity_type with
    | `Player _ -> Some e
    | _ -> None)
;;

let direction_deltas = function
  | `Up -> [ (-1, -1); (0, -1); (1, -1) ]
  | `Down -> [ (-1, 1); (0, 1); (1, 1) ]
  | `Left -> [ (-1, 1); (-1, 0); (-1, -1) ]
  | `Right -> [ (1, -1); (1, 0); (1, 1) ]
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
    ~f:(fun ~key:_ ~data:WireFormat.{ x; y; z; entity_type; _ } grid ->
      match entity_type with
      | `Environment `Wall -> Costs.add (x, y, z) 1000 grid
      | `Environment `Glass -> Costs.add (x, y, z) 1 grid
      | _ -> grid)
;;

let ray ~pos:(x, y) ~deltas ~energy ~propagation_costs =
  let travel_energy dx dy = abs dx + abs dy in
  let rec step deltas (x, y) energy acc visited =
    if energy <= 0 || Set2d.mem (x, y) visited
    then acc
    else (
      let new_visited = Set2d.add (x, y) visited in
      let new_energy =
        energy - (Costs.find_opt (x, y, 0) propagation_costs |> Option.value ~default:0)
      in
      let new_deltas =
        List.filter_map
          (fun (dx, dy) ->
            let next_x, next_y = (x + dx, y + dy) in
            let next_loss =
              Costs.find_opt (next_x, next_y, 0) propagation_costs
              |> Option.value ~default:0
            in
            if new_energy - travel_energy dx dy - next_loss > 0
            then Some (dx, dy)
            else None)
          deltas
      in
      let will_visit =
        List.fold_left
          (fun acc_ (dx, dy) ->
            step
              new_deltas
              (x + dx, y + dy)
              (new_energy - travel_energy dx dy)
              acc_
              new_visited)
          acc
          deltas
      in
      (x, y) :: will_visit)
  in
  step deltas (x, y) energy [ (x, y) ] Set2d.empty
;;

let visible_map id game =
  let WireFormat.{ x = px; y = py; z = pz; _ } = find_entity_exn game id in
  let energy = 10 in
  let propagation_costs = propagation_costs game.entities in
  let positions_to_send =
    [ `Up; `Down; `Left; `Right ]
    |> List.fold_left
         (fun acc dir ->
           let new_positions =
             ray ~pos:(px, py) ~deltas:(direction_deltas dir) ~energy ~propagation_costs
           in
           Set2d.union acc (Set2d.of_list new_positions))
         Set2d.empty
  in
  ( game.entities
    |> Base.Hashtbl.data
    |> List.filter (fun ({ x; y; z; _ } : entity) ->
      z = pz && Set2d.mem (x, y) positions_to_send),
    Theme.get_theme_by_index pz )
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
  if Set3d.mem (nx, ny, entity.z) walls
  then None
  else Some (update_entity game { entity with x = nx; y = ny })
;;

type game_ended =
  | Win of WireFormat.character_type
  | Other of string

let verify_end_conditions game start_time =
  let _all_zombie entities =
    not
    @@ Base.Hashtbl.exists entities ~f:(fun e ->
      match e.WireFormat.entity_type with
      | `Player `Human -> true
      | _ -> false)
  in
  let now = Unix.time () in
  if int_of_float (now -. start_time) >= game.config.time_limit
  then
    Some (Win `Human)
    (* else if all_zombie game.entities
       then Some (Win `Zombie) *)
  else None
;;
