type actions = Up | Down

let parse str =
  List.init (String.length str) (String.get str)
  |> List.map (fun s -> if s == '(' then Some Up else if s == ')' then Some Down else None)
  |> List.filter Option.is_some
  |> List.map Option.get

let eval_step lst value =
  match lst with
  | [] -> value
  | Up :: _ -> value + 1
  | Down :: _ -> value - 1

let rec eval lst value =
  match lst with
  | [] -> value
  | action :: rest -> eval rest @@ eval_step [ action ] value

let parse_and_eval str = eval (parse str) 0

let rec find_basement_entry_pos lst value pos =
  if value = -1 then pos
  else
    match lst with
    | [] -> -1
    | action :: rest -> find_basement_entry_pos rest (eval_step [ action ] value) (pos + 1)

let parse_and_find_basement_entry_pos str = find_basement_entry_pos (parse str) 0 0

let run input =
  print_endline @@ "The result of part 1 is: " ^ string_of_int @@ parse_and_eval input;
  print_endline @@ "The result of part 2 is: " ^ string_of_int @@ parse_and_find_basement_entry_pos input
