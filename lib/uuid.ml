module HashtblKey = Base.Int

let create_gen () = ref 0

let next_id next_id_gen =
  let id = !next_id_gen in
  Base.Int.incr next_id_gen;
  id
;;
