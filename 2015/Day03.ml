type coord = int * int
type direction = North | South | West | East

module CoordMap = struct
  include Map.Make (struct
    type t = coord

    let compare (x1, y1) (x2, y2) =
      match compare x1 x2 with
      | 0 -> compare y1 y2
      | res -> res
  end)

  let init_pos key map =
    update key
      (function
        | Some value -> Some value
        | None -> Some 1)
      map

  let increment key map =
    update key
      (function
        | Some value -> Some (value + 1)
        | None -> Some 1)
      map
end

let char_to_direction = function
  | '^' -> Some North
  | 'v' -> Some South
  | '>' -> Some East
  | '<' -> Some West
  | _ -> None

let deliver_present direction current visited =
  let x, y = current in
  let visited = CoordMap.init_pos current visited in
  match direction with
  | North ->
      let new_pos = (x, y - 1) in
      (CoordMap.increment new_pos visited, new_pos)
  | South ->
      let new_pos = (x, y + 1) in
      (CoordMap.increment new_pos visited, new_pos)
  | East ->
      let new_pos = (x + 1, y) in
      (CoordMap.increment new_pos visited, new_pos)
  | West ->
      let new_pos = (x - 1, y) in
      (CoordMap.increment new_pos visited, new_pos)

let rec deliver_present_path directions current visited =
  match directions with
  | [] -> (visited, current)
  | dir :: rest ->
      let visited, new_pos = deliver_present dir current visited in
      deliver_present_path rest new_pos visited

let parse str =
  List.init (String.length str) (String.get str)
  |> List.map char_to_direction
  |> List.filter Option.is_some
  |> List.map Option.get

let run input =
  print_endline
  @@ "The result of part 1 is: "
  ^ string_of_int
  @@ CoordMap.cardinal
  @@
  let visited, _ = deliver_present_path (parse input) (0, 0) CoordMap.empty in
  visited
