open Notty
open Notty_unix

let dot scale : image =
  I.uchar A.(bg black ++ fg (rgb ~r:scale ~g:0 ~b:0)) (Uchar.of_int 0x25cf) 1 1
;;

let hidden_background : image =
  I.uchar A.(fg white ++ bg lightblack) (Uchar.of_int 0x2591) 1 1
;;

let visible_background : image = I.uchar A.(bg black) (Uchar.of_char ' ') 1 1

module Set = Set.Make (struct
    type t = int * int

    let compare = Stdlib.compare
  end)

let terminal = Term.create ()

let fog_env distance =
  if distance > Game.view_radius_sq then hidden_background else visible_background
;;

let fog distance =
  if distance > Game.view_radius_sq
  then hidden_background
  else (
    let ratio = 1.0 -. (float_of_int distance /. float_of_int Game.view_radius_sq) in
    dot (int_of_float (5.0 *. ratio *. ratio)))
;;

let dist (ax, ay) (bx, by) =
  let dx, dy = (bx - ax, by - ay) in
  (dx * dx) + (dy * dy)
;;

let render ~me terminal Game.{ width; height; entities } =
  let entities_set = Set.of_list entities in
  let image =
    I.tabulate width height
    @@ fun x y ->
    let distance = dist (x, y) me in
    if Set.mem (x, y) entities_set then fog distance else fog_env distance
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
    render ~me:(List.hd updated_game.entities) terminal updated_game;
    main_loop terminal updated_game id
  | _ ->
    render ~me:(List.hd game.entities) terminal game;
    main_loop terminal game id
;;
