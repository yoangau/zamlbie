module WireFormat = struct
  include Game_t
  module Serializer = Game_j

  let wire_format ~game_id ~config ~entities = { game_id; config; entities }
end

let () = Random.self_init ()
let game_id_gen = Uuid.create_gen ()

type config = WireFormat.config
type entity = WireFormat.entity

type t =
  { game_id : int;
    entities : (Uuid.HashtblKey.t, entity) Base.Hashtbl.t;
    config : config
  }

let default_entity =
  WireFormat.{ id = 0; entity_type = `Player `Human; x = 0; y = 0; z = 0 }
;;

let is_entity a b = WireFormat.(a.id = b.id)
let find_entity { entities; _ } id = Base.Hashtbl.find entities id
let find_entity_exn { entities; _ } id = Base.Hashtbl.find_exn entities id

module Positions = Set.Make (struct
    type t = int * int * int

    let compare = Stdlib.compare
  end)

let gather_positions ~p ~entities =
  Base.Hashtbl.fold
    entities
    ~init:Positions.empty
    ~f:(fun ~key:_ ~data:WireFormat.{ entity_type; x; y; z; _ } positions ->
      if p entity_type then Positions.add (x, y, z) positions else positions)
;;

let partition_map id game =
  let WireFormat.{ x = px; y = py; z = pz; _ } = find_entity_exn game id in
  let positions_to_send =
    let w, h = (21, 21) in
    List.init w (fun x -> List.init h (fun y -> (px + x - (w / 2), py + y - (h / 2), pz)))
    |> List.flatten
    |> Positions.of_list
  in
  ( game.entities
    |> Base.Hashtbl.data
    |> List.filter (fun ({ x; y; z; _ } : entity) ->
      Positions.mem (x, y, z) positions_to_send),
    Theme.name_from_index pz )
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
  if Positions.mem (nx, ny, entity.z) walls
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
