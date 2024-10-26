let () =
  match Base.get_day () with
  | Some 1 -> Day01.run (Base.read_text "./2015/Day01_input.txt")
  | Some 2 -> Day02.run (Base.read_text "./2015/Day02_input.txt")
  | Some 3 -> Day03.run (Base.read_text "./2015/Day03_input.txt")
  | Some 4 -> Day04.run (Base.read_text "./2015/Day04_input.txt")
  | Some 5 -> Day05.run (Base.read_text "./2015/Day05_input.txt")
  | Some n -> print_endline ("Unknown day specified: " ^ string_of_int n)
  | None -> print_endline "Could not determine which day to run"
