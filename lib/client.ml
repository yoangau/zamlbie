open Notty
open Notty_unix

let dot : image = I.uchar A.(fg lightred) (Uchar.of_int 0x25cf) 1 1
let background : image = I.uchar A.(fg lightblue) (Uchar.of_int 0x25cf) 1 1

module Set = Set.Make (struct
    type t = int * int

    let compare = Stdlib.compare
  end)

let terminal = Term.create ()

let render terminal Game.{ width; height; entities } =
  let entities_set = Set.of_list entities in
  let image =
    I.tabulate width height
    @@ fun x y -> if Set.mem (x, y) entities_set then dot else background
  in
  Term.image terminal image
;;

let rec main_loop t game =
  Unix.sleepf 0.016;
  render t game;
  match Term.event t with
  | `End | `Key (`Escape, []) -> ()
  | `Key (`Arrow arrow, []) ->
    let dir =
      match arrow with
      | `Up -> (0, -1)
      | `Down -> (0, 1)
      | `Left -> (-1, 0)
      | `Right -> (1, 0)
    in
    let new_game = Game.move game 0 dir in
    main_loop t new_game
  | _ -> main_loop t game
;;
