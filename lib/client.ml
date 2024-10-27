open Notty

let dot : image = I.uchar A.(fg lightred) (Uchar.of_int 0x25cf) 1 1
let background : image = I.uchar A.(fg lightblue) (Uchar.of_int 0x25cf) 1 1

module Set = Set.Make (struct
    type t = int * int

    let compare = Stdlib.compare
  end)

let terminal = Notty_unix.Term.create ()

let render terminal Game.{ width; height; entities } =
  let entities_set = Set.of_list entities in
  let image =
    I.tabulate width height
    @@ fun x y -> if Set.mem (x, y) entities_set then dot else background
  in
  Notty_unix.Term.image terminal image
;;
