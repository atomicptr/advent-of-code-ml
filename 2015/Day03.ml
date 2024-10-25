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

  let init_pos key map value =
    update key
      (function
        | Some value -> Some value
        | None -> Some value)
      map

  let increment key map =
    update key
      (function
        | Some value -> Some (value + 1)
        | None -> Some 1)
      map

  let merge_values map1 map2 =
    merge
      (fun _ v1 v2 ->
        match (v1, v2) with
        | Some v1, Some v2 -> Some (v1 + v2)
        | Some v, None | None, Some v -> Some v
        | None, None -> None)
      map1 map2
end

let char_to_direction = function
  | '^' -> Some North
  | 'v' -> Some South
  | '>' -> Some East
  | '<' -> Some West
  | _ -> None

let deliver_present direction current visited =
  let x, y = current in
  let visited = CoordMap.init_pos current visited 1 in
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

let rec deliver_present_path_with_robot directions (current_santa, visited_santa) (current_robo, visited_robo) =
  match directions with
  | [] -> (visited_santa, visited_robo)
  | [ dir ] ->
      let visited_santa, new_pos = deliver_present dir current_santa visited_santa in
      deliver_present_path_with_robot [] (new_pos, visited_santa) (current_robo, visited_robo)
  | dir_santa :: dir_robot :: rest ->
      let visited_santa, santa_new_pos = deliver_present dir_santa current_santa visited_santa in
      let visisted_robo, robo_new_pos = deliver_present dir_robot current_robo visited_robo in
      deliver_present_path_with_robot rest (santa_new_pos, visited_santa) (robo_new_pos, visisted_robo)

let parse str =
  List.init (String.length str) (String.get str)
  |> List.map char_to_direction
  |> List.filter Option.is_some
  |> List.map Option.get

let run input =
  (print_endline
  @@ "The result of part 1 is: "
  ^ string_of_int
  @@ CoordMap.cardinal
  @@
  let visited, _ = deliver_present_path (parse input) (0, 0) CoordMap.empty in
  visited);
  print_endline
  @@ "The result of part 2 is: "
  ^ string_of_int
  @@ CoordMap.cardinal
  @@
  let santa, robot = deliver_present_path_with_robot (parse input) ((0, 0), CoordMap.empty) ((0, 0), CoordMap.empty) in
  CoordMap.merge_values santa robot
