type color =
  [ `Red
  | `Green
  | `Blue
  | `Yellow
  | `Cyan
  | `Magenta
  | `Orange
  | `Purple
  | `Pink
  | `Lime
  | `Teal
  | `Brown
  | `Gray
  | `White
  | `Black
  | `Gold
  ]

module ThemeMap = Map.Make (struct
    type t = Theme_t.theme_name

    let compare = compare
  end)

type rgb =
  { r : int;
    g : int;
    b : int
  }

type t =
  { background : rgb;
    human : rgb;
    zombie : rgb;
    wall : rgb;
    glass : rgb;
    stair_up : rgb;
    stair_down : rgb;
    fog : rgb
  }

let color_to_rgb = function
  | `Red -> { r = 255; g = 0; b = 0 }
  | `Green -> { r = 0; g = 255; b = 0 }
  | `Blue -> { r = 0; g = 0; b = 255 }
  | `Yellow -> { r = 255; g = 255; b = 0 }
  | `Cyan -> { r = 0; g = 255; b = 255 }
  | `Magenta -> { r = 255; g = 0; b = 255 }
  | `Orange -> { r = 255; g = 153; b = 0 }
  | `Purple -> { r = 153; g = 0; b = 153 }
  | `Pink -> { r = 255; g = 153; b = 204 }
  | `Lime -> { r = 102; g = 204; b = 102 }
  | `Teal -> { r = 0; g = 153; b = 153 }
  | `Brown -> { r = 153; g = 51; b = 51 }
  | `Gray -> { r = 153; g = 153; b = 153 }
  | `White -> { r = 255; g = 255; b = 255 }
  | `Black -> { r = 0; g = 0; b = 0 }
  | `Gold -> { r = 255; g = 204; b = 0 }
;;

let default_theme =
  ( `Default,
    { background = color_to_rgb `Black;
      human = color_to_rgb `Red;
      zombie = color_to_rgb `Green;
      wall = color_to_rgb `White;
      glass = color_to_rgb `Blue;
      stair_up = color_to_rgb `Cyan;
      stair_down = color_to_rgb `Magenta;
      fog = color_to_rgb `Gray
    } )
;;

let themes_list =
  [ default_theme;
    ( `Fire,
      { background = color_to_rgb `Red;
        human = color_to_rgb `Yellow;
        zombie = color_to_rgb `Orange;
        wall = color_to_rgb `Brown;
        glass = color_to_rgb `Cyan;
        stair_up = color_to_rgb `Gold;
        stair_down = color_to_rgb `Purple;
        fog = color_to_rgb `Gray
      } );
    ( `Ocean,
      { background = color_to_rgb `Blue;
        human = color_to_rgb `Cyan;
        zombie = color_to_rgb `Teal;
        wall = color_to_rgb `Gray;
        glass = color_to_rgb `White;
        stair_up = color_to_rgb `Yellow;
        stair_down = color_to_rgb `Black;
        fog = color_to_rgb `Blue
      } );
    ( `Forest,
      { background = color_to_rgb `Green;
        human = color_to_rgb `Lime;
        zombie = color_to_rgb `Brown;
        wall = color_to_rgb `Teal;
        glass = color_to_rgb `White;
        stair_up = color_to_rgb `Orange;
        stair_down = color_to_rgb `Gray;
        fog = color_to_rgb `Black
      } );
    ( `Volcano,
      { background = color_to_rgb `Orange;
        human = color_to_rgb `Red;
        zombie = color_to_rgb `Gold;
        wall = color_to_rgb `Brown;
        glass = color_to_rgb `Yellow;
        stair_up = color_to_rgb `Purple;
        stair_down = color_to_rgb `Gray;
        fog = color_to_rgb `Black
      } );
    ( `Candy,
      { background = color_to_rgb `Pink;
        human = color_to_rgb `White;
        zombie = color_to_rgb `Purple;
        wall = color_to_rgb `Lime;
        glass = color_to_rgb `Yellow;
        stair_up = color_to_rgb `Cyan;
        stair_down = color_to_rgb `Gold;
        fog = color_to_rgb `Gray
      } );
    ( `Desert,
      { background = color_to_rgb `Gold;
        human = color_to_rgb `Orange;
        zombie = color_to_rgb `Brown;
        wall = color_to_rgb `Yellow;
        glass = color_to_rgb `Teal;
        stair_up = color_to_rgb `Red;
        stair_down = color_to_rgb `Gray;
        fog = color_to_rgb `White
      } );
    ( `Ice,
      { background = color_to_rgb `Cyan;
        human = color_to_rgb `Blue;
        zombie = color_to_rgb `Teal;
        wall = color_to_rgb `White;
        glass = color_to_rgb `Gray;
        stair_up = color_to_rgb `Yellow;
        stair_down = color_to_rgb `Black;
        fog = color_to_rgb `Cyan
      } );
    ( `Twilight,
      { background = color_to_rgb `Purple;
        human = color_to_rgb `Pink;
        zombie = color_to_rgb `Blue;
        wall = color_to_rgb `Gray;
        glass = color_to_rgb `Gold;
        stair_up = color_to_rgb `White;
        stair_down = color_to_rgb `Black;
        fog = color_to_rgb `Purple
      } );
    ( `Galaxy,
      { background = color_to_rgb `Black;
        human = color_to_rgb `White;
        zombie = color_to_rgb `Purple;
        wall = color_to_rgb `Blue;
        glass = color_to_rgb `Magenta;
        stair_up = color_to_rgb `Gold;
        stair_down = color_to_rgb `Yellow;
        fog = color_to_rgb `Gray
      } );
    ( `Sunset,
      { background = color_to_rgb `Yellow;
        human = color_to_rgb `Orange;
        zombie = color_to_rgb `Red;
        wall = color_to_rgb `Pink;
        glass = color_to_rgb `Purple;
        stair_up = color_to_rgb `Gray;
        stair_down = color_to_rgb `Black;
        fog = color_to_rgb `Brown
      } );
    ( `Cyber,
      { background = color_to_rgb `Teal;
        human = color_to_rgb `Lime;
        zombie = color_to_rgb `Cyan;
        wall = color_to_rgb `White;
        glass = color_to_rgb `Yellow;
        stair_up = color_to_rgb `Blue;
        stair_down = color_to_rgb `Gray;
        fog = color_to_rgb `Black
      } );
    ( `Mystic,
      { background = color_to_rgb `Magenta;
        human = color_to_rgb `Purple;
        zombie = color_to_rgb `Pink;
        wall = color_to_rgb `Gray;
        glass = color_to_rgb `Gold;
        stair_up = color_to_rgb `Teal;
        stair_down = color_to_rgb `Blue;
        fog = color_to_rgb `Black
      } );
    ( `Jungle,
      { background = color_to_rgb `Lime;
        human = color_to_rgb `Green;
        zombie = color_to_rgb `Brown;
        wall = color_to_rgb `Gray;
        glass = color_to_rgb `Yellow;
        stair_up = color_to_rgb `Orange;
        stair_down = color_to_rgb `Gold;
        fog = color_to_rgb `Black
      } );
    ( `Cave,
      { background = color_to_rgb `Brown;
        human = color_to_rgb `Yellow;
        zombie = color_to_rgb `Orange;
        wall = color_to_rgb `Gray;
        glass = color_to_rgb `Black;
        stair_up = color_to_rgb `Gold;
        stair_down = color_to_rgb `Purple;
        fog = color_to_rgb `Blue
      } );
    ( `Aurora,
      { background = color_to_rgb `White;
        human = color_to_rgb `Cyan;
        zombie = color_to_rgb `Blue;
        wall = color_to_rgb `Gray;
        glass = color_to_rgb `Magenta;
        stair_up = color_to_rgb `Purple;
        stair_down = color_to_rgb `Black;
        fog = color_to_rgb `White
      } );
    ( `Void,
      { background = color_to_rgb `Gray;
        human = color_to_rgb `Black;
        zombie = color_to_rgb `White;
        wall = color_to_rgb `Blue;
        glass = color_to_rgb `Purple;
        stair_up = color_to_rgb `Gold;
        stair_down = color_to_rgb `Red;
        fog = color_to_rgb `Black
      } )
  ]
;;

let themes = ThemeMap.of_list themes_list
let from_name name = ThemeMap.find name themes
let theme_count = List.length themes_list

let name_from_index index =
  let theme_index = index mod theme_count in
  let theme_name, _ = List.nth themes_list theme_index in
  theme_name
;;
