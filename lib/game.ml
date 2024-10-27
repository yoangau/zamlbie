type game =
  { entities : (int * int) list;
    width : int;
    height : int
  }

type move =
  [ `Up
  | `Down
  | `Left
  | `Right
  ]

let get_move_delta = function
  | `Up -> (0, -1)
  | `Down -> (0, 1)
  | `Left -> (-1, 0)
  | `Right -> (1, 0)
;;

let move { entities; width; height } entity_index move =
  let dx, dy = get_move_delta move in
  let x, y = List.nth entities entity_index in
  let new_entity =
    ( Base.Int.clamp_exn (x + dx) ~min:0 ~max:(width - 1),
      Base.Int.clamp_exn (y + dy) ~min:0 ~max:(height - 1) )
  in
  { width;
    height;
    entities =
      List.mapi (fun i entity -> if i = entity_index then new_entity else entity) entities
  }
;;

let init width height = { entities = [ (0, 0); (width - 1, height - 1) ]; width; height }
