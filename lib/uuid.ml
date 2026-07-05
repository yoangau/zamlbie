module HashtblKey = Base.Int

(* Generators are shared between domains (e.g. entity ids are drawn from a
   single global generator by every match orchestrator), so they must be
   atomic. *)
let create_gen () = Atomic.make 0
let next_id next_id_gen = Atomic.fetch_and_add next_id_gen 1
