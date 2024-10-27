open Notty
open Notty_unix

let dot scale id : image =
  I.uchar A.(bg black ++ fg (rgb ~r:scale ~g:0 ~b:0)) (Uchar.of_int (0x278A + id)) 1 1
;;

let hidden_background i : image =
  I.uchar A.(fg lightblack ++ bg black) (Uchar.of_int (0x2591 + i)) 1 1
;;

let visible_background : image = I.uchar A.(bg black) (Uchar.of_char ' ') 1 1

module Map = Map.Make (struct
    type t = int * int

    let compare = Stdlib.compare
  end)

let terminal = Term.create ()

let fog_env distance =
  if distance = Game.view_radius_sq
  then hidden_background 0
  else if distance > Game.view_radius_sq
  then hidden_background 1
  else visible_background
;;

let fog distance player =
  if distance = Game.view_radius_sq
  then hidden_background 0
  else if distance > Game.view_radius_sq
  then hidden_background 1
  else (
    let ratio = 1.0 -. (float_of_int distance /. float_of_int Game.view_radius_sq) in
    dot (int_of_float (5.0 *. ratio *. ratio)) player)
;;

let dist (ax, ay) (bx, by) =
  let dx, dy = (bx - ax, by - ay) in
  (dx * dx) + (dy * dy)
;;

let render ~me:Game.{ x = mx; y = my; id; _ } terminal Game.{ width; height; entities } =
  let entities_set =
    Map.of_list (List.map (fun entity -> Game.((entity.x, entity.y), entity)) entities)
  in
  let image =
    I.tabulate width height
    @@ fun x y ->
    let distance = dist (x, y) (mx, my) in
    if Map.mem (x, y) entities_set then fog distance id else fog_env distance
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

let rec main_loop terminal game game_id =
  Unix.sleepf 0.016;
  match get_player_input terminal with
  | Some Exit -> ()
  | Some (Move direction) ->
    Server.on_player_input ~game_id ~player_id:0 direction |> ignore;
    let updated_game = Server.on_game_update ~game_id in
    render ~me:(List.hd updated_game.entities) terminal updated_game;
    main_loop terminal updated_game game_id
  | _ ->
    render ~me:(List.hd game.entities) terminal game;
    main_loop terminal game game_id
;;
