open Notty

let solid color : image = I.uchar A.(bg color) (Uchar.of_char ' ') 1 1

let symbol ~foreground ~background ~char_of_int : image =
  I.uchar A.(fg foreground ++ bg background) (Uchar.of_int char_of_int) 1 1
;;

let stairs_up foreground background : image =
  symbol ~foreground ~background ~char_of_int:0x2B06
;;

let stairs_down foreground background : image =
  symbol ~foreground ~background ~char_of_int:0x2B07
;;

let hidden_background foreground background i : image =
  symbol ~foreground ~background ~char_of_int:(0x2591 + i)
;;

let scale_channel channel scale =
  let c = float_of_int channel in
  int_of_float (c *. scale)
;;

let notty_color_of_rgb ?alpha (rgb : Theme.rgb) =
  match alpha with
  | None -> Notty.A.rgb ~r:rgb.r ~g:rgb.g ~b:rgb.b
  | Some alpha ->
    let a = Base.Float.clamp_exn alpha ~min:0.0 ~max:1.0 in
    Notty.A.rgb
      ~r:(scale_channel rgb.r a)
      ~g:(scale_channel rgb.g a)
      ~b:(scale_channel rgb.b a)
;;

let render_tile ?(alpha = 1.0) theme_name tile =
  let theme = Theme.get_theme_by_name theme_name in
  match tile with
  | `Player `Human -> solid (notty_color_of_rgb theme.human)
  | `Player `Zombie -> solid (notty_color_of_rgb theme.zombie ~alpha)
  | `Environment `Wall -> solid (notty_color_of_rgb theme.wall)
  | `Environment `Glass -> solid (notty_color_of_rgb theme.glass)
  | `Environment `StairsUp ->
    stairs_up (notty_color_of_rgb theme.stair_up) (notty_color_of_rgb theme.background)
  | `Environment `StairsDown ->
    stairs_down
      (notty_color_of_rgb theme.stair_down)
      (notty_color_of_rgb theme.background)
  | `Environment `Fog -> solid (notty_color_of_rgb theme.fog ~alpha)
  | `Environment (`Hidden i) ->
    hidden_background
      (notty_color_of_rgb theme.fog)
      (notty_color_of_rgb theme.background)
      i
  | `Environment `Visible -> solid (notty_color_of_rgb theme.background)
;;
