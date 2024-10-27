open Notty
open Notty_unix

let dot ratio : image =
  let scale = 5.0 *. Base.Float.clamp_exn ratio ~min:0.0 ~max:1.0 in
  I.uchar A.(bg (rgb ~r:(int_of_float scale) ~g:0 ~b:0)) (Uchar.of_char ' ') 1 1
;;

let hidden_background i : image =
  I.uchar A.(fg lightblack ++ bg black) (Uchar.of_int (0x2591 + i)) 1 1
;;

let background ratio : image =
  let scale = 23.0 *. Base.Float.clamp_exn ratio ~min:0.0 ~max:1.0 in
  I.uchar A.(bg (gray @@ int_of_float scale)) (Uchar.of_int 0x2591) 1 1
;;

let visible_background : image = I.uchar A.(bg black) (Uchar.of_char ' ') 1 1

module Map = Map.Make (struct
    type t = int * int

    let compare = Stdlib.compare
  end)

let terminal = Term.create ()

let fog_env distance_sq =
  let view_radius_sq = float_of_int Game.view_radius_sq in
  if distance_sq > view_radius_sq
  then (
    let distance = sqrt distance_sq in
    let view_radius = view_radius_sq |> sqrt in
    background @@ ((distance -. view_radius) /. 20.0))
  else if distance_sq = view_radius_sq
  then hidden_background 0
  else visible_background
;;

let render_agent distance =
  let ratio = 1.0 -. (float_of_int distance /. float_of_int Game.view_radius_sq) in
  dot (ratio *. ratio)
;;

let dist_sq (ax, ay) (bx, by) =
  let dx, dy = (bx - ax, by - ay) in
  (dx * dx) + (dy * dy)
;;

let is_visible distance_sq = distance_sq <= Game.view_radius_sq

let render ~me:Game.{ x = mx; y = my; _ } terminal Game.{ width; height; entities } =
  let entities_set =
    Map.of_list (List.map (fun entity -> Game.((entity.x, entity.y), entity)) entities)
  in
  let image =
    I.tabulate (width * 2) height
    @@ fun x y ->
    let x = x / 2 in
    let distance_from_player = dist_sq (x, y) (mx, my) in
    if Map.mem (x, y) entities_set && is_visible distance_from_player
    then render_agent distance_from_player
    else fog_env (float_of_int distance_from_player)
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
