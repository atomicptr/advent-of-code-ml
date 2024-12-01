let find_min l =
  let rec inner min other = function
    | [] -> (min, other)
    | elem :: rest when elem < min -> inner elem (min :: other) rest
    | elem :: rest -> inner min (elem :: other) rest
  in
  match l with
  | [] -> (None, [])
  | hd :: tl ->
      let min, other = inner hd [] tl in
      (Some min, other)

let rec total_distance lst1 lst2 =
  let min1, rest1 = find_min lst1 in
  let min2, rest2 = find_min lst2 in
  match (min1, min2) with
  | None, _ -> 0
  | _, None -> 0
  | Some min1, Some min2 -> Int.abs (min1 - min2) + total_distance rest1 rest2

let parse_line str =
  let parts = String.split_on_char ' ' str |> List.filter (fun part -> String.length part > 0) in
  (int_of_string @@ List.nth parts 0, int_of_string @@ List.nth parts 1)

let parse_input str =
  let rec inner acc1 acc2 = function
    | [] -> (acc1, acc2)
    | line :: rest ->
        let elem1, elem2 = parse_line line in
        inner (elem1 :: acc1) (elem2 :: acc2) rest
  in
  inner [] [] (Base.split_on_newline str)

let run input =
  let lst1, lst2 = parse_input input in
  print_endline @@ "Part 1: " ^ string_of_int @@ total_distance lst1 lst2
