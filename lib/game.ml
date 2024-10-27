include Game_t
module Serializer = Game_j

let get_move_delta = function
  | `Up -> (0, -1)
  | `Down -> (0, 1)
  | `Left -> (-1, 0)
  | `Right -> (1, 0)
;;

let default_entity = { id = 0; entity_type = `Player; x = 0; y = 0 }
let view_radius_sq = 5 * 5
let is_entity a b = a.id = b.id && a.entity_type = b.entity_type

let find_entity { entities; _ } id entity_type =
  List.find_opt (is_entity { default_entity with id; entity_type }) entities
;;

let update_entity game new_entity =
  { game with
    entities =
      List.map
        (fun entity -> if is_entity entity new_entity then new_entity else entity)
        game.entities
  }
;;

let move ~game ~id ~entity_type ~move =
  let ( let* ) = Base.Option.( >>= ) in
  let* entity = find_entity game id entity_type in
  let dx, dy = get_move_delta move in
  let nx, ny =
    ( Base.Int.clamp_exn (entity.x + dx) ~min:0 ~max:(game.width - 1),
      Base.Int.clamp_exn (entity.y + dy) ~min:0 ~max:(game.height - 1) )
  in
  Some (update_entity game { entity with x = nx; y = ny })
;;

let init width height =
  { entities =
      [ default_entity; { default_entity with id = 1; x = width - 1; y = height - 1 } ];
    width;
    height
  }
;;
