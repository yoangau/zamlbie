type game =
  { entities : (int * int) list;
    width : int;
    height : int
  }

let init width height = { entities = [ (0, 0); (width - 1, height - 1) ]; width; height }
