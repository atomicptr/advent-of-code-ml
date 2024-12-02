type report_state = Unknown | Increasing | Decreasing

let number_state a b = if a < b then Increasing else Decreasing
let rule1 prev_state a b = prev_state = number_state a b

let rule2 a b =
  let res = abs (a - b) in
  res >= 1 && res <= 3

let is_report_safe report =
  let rec inner state prev report =
    match (state, prev, report) with
    | Unknown, _, [] -> failwith "Invalid: Empty list"
    | Unknown, _, [ _ ] -> failwith "Invalid: Only one item"
    | Unknown, _, a :: b :: rest -> inner (number_state a b) a (b :: rest)
    | _, prev, hd :: _ when prev = hd -> false (* all must either increment or decrement so equals is also invalid *)
    | state, prev, hd :: _ when Bool.not @@ rule1 state prev hd -> false
    | _, prev, hd :: _ when Bool.not @@ rule2 prev hd -> false
    | _, prev, hd :: rest -> inner (number_state prev hd) hd rest
    | _, _, [] -> true
  in
  inner Unknown 0 report

let parse_report str = str |> String.split_on_char ' ' |> List.map int_of_string
let parse_data str = str |> Base.split_on_newline |> List.map parse_report

let run input =
  let data = parse_data input in
  let res = data |> List.map is_report_safe |> List.fold_left (fun acc item -> if item then acc + 1 else acc) 0 in
  print_endline @@ "Part 1: " ^ string_of_int res
