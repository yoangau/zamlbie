include Game_t
module Serializer = Game_j

let get_move_delta = function
  | `Up -> (0, -1)
  | `Down -> (0, 1)
  | `Left -> (-1, 0)
  | `Right -> (1, 0)
;;

let default_entity = { id = 0; entity_type = `Player `Human; x = 0; y = 0 }
let is_entity a b = a.id = b.id
let find_entity { entities; _ } id = List.find_opt (fun a -> a.id = id) entities
let add_entity game entity = { game with entities = entity :: game.entities }

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

let zombie_sortition game = 
 let player_ids = List.filter_map (fun e -> match e.entity_type with | `Player `Human -> Some (e) | _ -> None) game.entities in 
 let player_count = List.length player_ids in 
 let random_player_idx = Random.int (player_count + 1) in 
 let player_zombie_to_be= List.nth player_ids random_player_idx in 	
 update_entity game {player_zombie_to_be with entity_type = `Player `Zombie}


let default_config =
  { human_view_radius = 5;
    zombie_view_radius = 5;
    width = 20;
    height = 20;
    max_player_count = 2;
    time_limit = 60;
    tick_delta = 0.5
  }
;;

let make game_id config = { game_id; entities = []; config }
