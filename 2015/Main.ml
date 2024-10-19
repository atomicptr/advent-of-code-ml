let () = match Base.get_day() with
| Some 1 -> Day01.run ()
| Some n -> print_endline ("Unknown day specified: " ^ (string_of_int n))
| None -> print_endline "Could not determine which day to run"
