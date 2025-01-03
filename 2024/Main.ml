let () =
  match Base.get_day () with
  | Some 1 -> Day01.run (Base.read_text "./2024/Day01_input.txt")
  | Some 2 -> Day02.run (Base.read_text "./2024/Day02_input.txt")
  | Some n -> print_endline ("Unknown day specified: " ^ string_of_int n)
  | None -> print_endline "Could not determine which day to run"
