open Notty

let solid color : image = I.uchar A.(bg color) (Uchar.of_char ' ') 1 1

let symbol foreground background ~char_of_int : image =
  I.uchar A.(fg foreground ++ bg background) (Uchar.of_int char_of_int) 1 1
;;

let stairs_up = symbol ~char_of_int:0x2B06
let stairs_down = symbol ~char_of_int:0x2B07
let hidden_background ~intensity = symbol ~char_of_int:(0x2591 + intensity)

let scale_channel channel scale =
  let c = float_of_int channel in
  int_of_float (c *. scale)
;;

let notty_color_of_rgb ?alpha ({ r; g; b } : Theme.rgb) =
  match alpha with
  | None -> Notty.A.rgb_888 ~r ~g ~b
  | Some alpha ->
    let a = Base.Float.clamp_exn alpha ~min:0.0 ~max:1.0 in
    Notty.A.rgb_888 ~r:(scale_channel r a) ~g:(scale_channel g a) ~b:(scale_channel b a)
;;

type tile =
  [ Game.WireFormat.character_type
  | Game.WireFormat.environment_type
  | `Hidden of int
  | `Visible
  | `Fog
  ]

let render_tile ?alpha (theme : Theme.t) (tile : tile) =
  match tile with
  | `Human -> solid (notty_color_of_rgb theme.human)
  | `Zombie -> solid (notty_color_of_rgb theme.zombie ?alpha)
  | `Wall -> solid (notty_color_of_rgb theme.wall)
  | `Glass -> solid (notty_color_of_rgb theme.glass)
  | `StairsUp ->
    stairs_up (notty_color_of_rgb theme.stair_up) (notty_color_of_rgb theme.background)
  | `StairsDown ->
    stairs_down
      (notty_color_of_rgb theme.stair_down)
      (notty_color_of_rgb theme.background)
  | `Fog -> solid (notty_color_of_rgb theme.fog ?alpha)
  | `Hidden intensity ->
    hidden_background
      ~intensity
      (notty_color_of_rgb theme.fog)
      (notty_color_of_rgb theme.background)
  | `Visible -> solid (notty_color_of_rgb theme.background)
;;
