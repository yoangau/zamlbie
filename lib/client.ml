open Notty
open Notty_unix

let dot : image = I.uchar A.(bg lightblue ++ fg lightred) (Uchar.of_int 0x25cf) 1 1
let background : image = I.uchar A.(bg lightblue) (Uchar.of_char ' ') 1 1

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

type input =
  | Exit
  | Move of Game.move

let get_player_input terminal =
  match Term.event terminal with
  | `End | `Key (`Escape, []) -> Some Exit
  | `Key (`Arrow arrow, []) -> Some (Move arrow)
  | _ -> None
;;

let rec main_loop terminal game id =
  Unix.sleepf 0.016;
  match get_player_input terminal with
  | Some Exit -> ()
  | Some (Move direction) ->
    Server.on_player_input ~id ~player:0 direction;
    let updated_game = Server.on_game_update ~id in
    render terminal updated_game;
    main_loop terminal updated_game id
  | _ ->
    render terminal game;
    main_loop terminal game id
;;
