let calculate_area a b = a * b
let calculate_surface_area l w h = (2 * calculate_area l w) + (2 * calculate_area w h) + (2 * calculate_area h l)

let calculate_extra_paper l w h =
  List.fold_left min Int.max_int [ calculate_area l w; calculate_area w h; calculate_area h l ]

let calculate l w h = calculate_surface_area l w h + calculate_extra_paper l w h
let to_string l w h = string_of_int l ^ "x" ^ string_of_int w ^ "x" ^ string_of_int h

let parse_line str =
  String.split_on_char 'x' str |> List.filter (fun s -> String.length s > 0) |> List.map int_of_string

let parse_file str calc_func =
  String.split_on_char '\n' str
  |> List.filter (fun s -> String.length s > 0)
  |> List.map parse_line
  |> List.map (fun lst -> calc_func (List.nth lst 0) (List.nth lst 1) (List.nth lst 2))
  |> List.fold_left ( + ) 0

let calculate_ribbon_wrapping l w h = 2 * List.fold_left min Int.max_int [ l + w; l + h; w + h ]
let calculate_ribbon_bow l w h = l * w * h
let calculate_ribbon l w h = calculate_ribbon_wrapping l w h + calculate_ribbon_bow l w h

let run input =
  print_endline @@ "The result of part 1 is: " ^ string_of_int @@ parse_file input calculate;
  print_endline @@ "The result of part 2 is: " ^ string_of_int @@ parse_file input calculate_ribbon
