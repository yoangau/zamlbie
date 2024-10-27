open Notty
open Notty_unix
open Lwt.Infix

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

(* let terminal = Term.create () *)

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

let ( let+ ) = Lwt.bind

let receive socket =
  let open Lwt.Infix in
  Hyper.receive socket
  >>= function
  | Some message ->
    Lwt_result.return @@ Message.Serializer.server_message_of_string message
  | None -> Lwt_result.fail "No message received"
;;

let send message socket =
  Hyper.send socket @@ Message.Serializer.string_of_client_message message
;;

let rec main_loop ~terminal ~game ~socket =
  render ~me:Game.(List.hd game.entities) terminal game;
  render ~me:Game.(List.hd game.entities) terminal game;
  match get_player_input terminal with
  | Some Exit -> Lwt_result.return ()
  | Some (Move direction) ->
    send (`Move direction) socket |> ignore;
    receive socket
    >>= (function
     | Ok (`Update updated_game) ->
       print_endline "received game";
       main_loop ~terminal ~game:updated_game ~socket
     | _ -> main_loop ~terminal ~game ~socket)
  | None ->
    Unix.sleepf 0.1;
    main_loop ~terminal ~game ~socket
;;

let run () =
  let terminal = Term.create () in
  Lwt_main.run
    (let+ maybe_websocket = Hyper.websocket "ws://127.0.0.1:8080/join/0" in
     let websocket = Result.get_ok maybe_websocket in
     receive websocket
     >>= function
     | Ok (`Update game) -> main_loop ~terminal ~game ~socket:websocket
     | _ -> Lwt_result.fail "Unexpected message")
  |> ignore;
  Some ()
;;
