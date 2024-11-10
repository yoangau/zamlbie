let next_id_gen = ref 0

let next_id () =
  let id = !next_id_gen in
  Base.Int.incr next_id_gen;
  id
;;
