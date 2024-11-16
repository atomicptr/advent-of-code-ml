type coord = int * int
type action = TurnOn of coord * coord | TurnOff of coord * coord | Toggle of coord * coord

let parse str =
  let parse_coord str =
    String.split_on_char ',' str |> List.map (fun s -> int_of_string s) |> function
    | [ a; b ] -> Some (a, b)
    | _ -> None
  in
  match String.split_on_char ' ' str with
  | [ "turn"; "on"; start_str; "through"; end_str ] ->
      Some (TurnOn (parse_coord start_str |> Option.get, parse_coord end_str |> Option.get))
  | [ "turn"; "off"; start_str; "through"; end_str ] ->
      Some (TurnOff (parse_coord start_str |> Option.get, parse_coord end_str |> Option.get))
  | [ "toggle"; start_str; "through"; end_str ] ->
      Some (Toggle (parse_coord start_str |> Option.get, parse_coord end_str |> Option.get))
  | _ -> None

type grid = bool array * int

let make_grid_opt width height =
  match (width, height) with
  | width, height when width <= 0 || height <= 0 -> None
  | width, height -> Some (Array.init (width * height) (fun _ -> false), width)

let make_grid width height = make_grid_opt width height |> Option.get
let grid_index (_, width) x y = (y * width) + x

let grid_get_opt (grid, width) x y =
  match (x, y) with
  | x, y when x < 0 || x > width || y < 0 || y > Array.length grid / width -> None
  | x, y -> Some grid.(grid_index (grid, width) x y)

let grid_get grid x y = grid_get_opt grid x y |> Option.get

let grid_set (grid, width) x y value =
  match (x, y) with
  | x, y when x < 0 || x > width || y < 0 || y > Array.length grid / width -> (grid, width)
  | x, y -> (Array.mapi (fun index v -> if index == (y * width) + x then value else v) grid, width)

let grid_apply (grid, width) (from_x, from_y) (to_x, to_y) func =
  let iter index v =
    let x = index mod width in
    let y = index / width in
    match (x, y) with
    | x, y when x < from_x || y < from_y -> v
    | x, y when x > to_x || y > to_y -> v
    | x, y when x >= from_x && y >= from_y -> func v
    | _ -> v
  in
  (Array.mapi iter grid, width)

let grid_set_range grid from_coord to_coord value = grid_apply grid from_coord to_coord (fun _ -> value)
let grid_flip_range grid from_coord to_coord = grid_apply grid from_coord to_coord (fun value -> Bool.not value)
let grid_count (grid, _) = Array.fold_left (fun acc curr -> if curr then acc + 1 else acc) 0 grid

let eval grid = function
  | TurnOn (from_coord, to_coord) -> grid_set_range grid from_coord to_coord true
  | TurnOff (from_coord, to_coord) -> grid_set_range grid from_coord to_coord false
  | Toggle (from_coord, to_coord) -> grid_flip_range grid from_coord to_coord

let rec eval_lines grid = function
  | [] -> grid_count grid
  | action :: rest -> eval_lines (eval grid (parse action |> Option.get)) rest

let run input =
  print_endline @@ "Part 1: " ^ string_of_int @@ eval_lines (make_grid 1000 1000) (Base.split_on_newline input)
