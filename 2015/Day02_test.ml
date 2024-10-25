open Alcotest

let check_surface_area l w h expected =
  let result = Day02.calculate_surface_area l w h in
  check int ("calculate_surface_area: " ^ Day02.to_string l w h) expected result

let check_extra_paper l w h expected =
  let result = Day02.calculate_extra_paper l w h in
  check int ("calculate_extra_paper: " ^ Day02.to_string l w h) expected result

let check_calc l w h expected =
  let result = Day02.calculate l w h in
  check int (Day02.to_string l w h) expected result

let check_parse_line str expected =
  let result = Day02.parse_line str in
  check (list int) ("parse_line: " ^ str) expected result

let check_parse_file str expected =
  let result = Day02.parse_file str Day02.calculate in
  check int ("parse_file: " ^ str) expected result

let check_calculate_ribbon l w h expected =
  let result = Day02.calculate_ribbon l w h in
  check int ("calculate_ribbon: " ^ Day02.to_string l w h) expected result

let test () =
  check_surface_area 2 3 4 52;
  check_extra_paper 2 3 4 6;
  check_calc 2 3 4 58;
  check_surface_area 1 1 10 42;
  check_extra_paper 1 1 10 1;
  check_calc 1 1 10 43;
  check_parse_line "2x3x4" [ 2; 3; 4 ];
  check_parse_line "1x1x10" [ 1; 1; 10 ];
  check_parse_file "2x3x4\n1x1x10\n" 101;
  check_calculate_ribbon 2 3 4 34;
  check_calculate_ribbon 1 1 10 14

let () = Alcotest.run "2015-02" [ ("Day 02", [ ("can run day 02 examples", `Quick, test) ]) ]
