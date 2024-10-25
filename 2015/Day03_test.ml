open Alcotest

let check_delivered dir expected =
  let _, new_pos = Day03.deliver_present dir (0, 0) Day03.CoordMap.empty in
  check (pair int int) "check deliviered" expected new_pos

let check_delivered_path directions expected =
  let _, new_pos = Day03.deliver_present_path directions (0, 0) Day03.CoordMap.empty in
  check (pair int int) "check deliviered path" expected new_pos

let test () =
  check_delivered North (0, -1);
  check_delivered South (0, 1);
  check_delivered West (-1, 0);
  check_delivered East (1, 0);
  check_delivered_path [ North; North; South; South ] (0, 0)

let () = Alcotest.run "2015-03" [ ("Day 03", [ ("can run day 03 examples", `Quick, test) ]) ]
