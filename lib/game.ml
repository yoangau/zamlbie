type game =
  { entities : (int * int) list;
    width : int;
    height : int
  }

let move { entities; width; height } entity_index (dx, dy) =
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
