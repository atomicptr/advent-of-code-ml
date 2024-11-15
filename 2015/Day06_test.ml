open Alcotest

let coord_to_string (x, y) = string_of_int x ^ "," ^ string_of_int y

let action_to_string = function
  | Day06.TurnOn (from_coord, to_coord) ->
      "turn on " ^ coord_to_string from_coord ^ " through " ^ coord_to_string to_coord
  | Day06.TurnOff (from_coord, to_coord) ->
      "turn off " ^ coord_to_string from_coord ^ " through " ^ coord_to_string to_coord
  | Day06.Toggle (from_coord, to_coord) ->
      "toggle " ^ coord_to_string from_coord ^ " through " ^ coord_to_string to_coord

let check_action expected =
  let result = Day06.parse expected |> Option.get in
  check string expected expected (action_to_string result)

let rec check_lights index grid expected = function
  | [] -> check int ("check lights #" ^ string_of_int @@ index) expected (Day06.grid_count grid)
  | action :: rest ->
      let res = Day06.eval grid action in
      check_lights index res expected rest

let test () =
  check_action "turn on 0,0 through 999,999";
  check_action "turn off 0,0 through 999,999";
  check_action "toggle 0,0 through 999,999";

  let grid = Day06.make_grid 3 3 in
  let grid = Day06.grid_set grid 1 1 true in
  check bool "check 1x1 grid" true (Day06.grid_get grid 1 1);

  check_lights 1 (Day06.make_grid 3 3) 0 [];
  check_lights 2 (Day06.make_grid 3 3) 1 [ Day06.TurnOn ((0, 0), (0, 0)) ];
  check_lights 2 (Day06.make_grid 3 3) 1 [ Day06.TurnOn ((1, 1), (1, 1)) ];
  check_lights 3 (Day06.make_grid 10 10) 100 [ Day06.TurnOn ((0, 0), (10, 10)) ];
  check_lights 4 (Day06.make_grid 10 10) 6 [ Day06.TurnOn ((0, 0), (5, 0)) ];
  check_lights 5 (Day06.make_grid 10 10) 3 [ Day06.TurnOn ((0, 0), (0, 2)) ];
  check_lights 6 (Day06.make_grid 1000 1000) 1000000 [ Day06.TurnOn ((0, 0), (999, 999)) ];
  check_lights 7 (Day06.make_grid 1000 1000) 4 [ Day06.TurnOn ((499, 499), (500, 500)) ];
  check_lights 8 (Day06.make_grid 1000 1000) (1000000 - 1000)
    [ Day06.TurnOn ((0, 0), (999, 999)); Day06.Toggle ((0, 0), (999, 0)) ];
  check_lights 9 (Day06.make_grid 1000 1000)
    (1000000 - 1000 - 4)
    [ Day06.TurnOn ((0, 0), (999, 999)); Day06.Toggle ((0, 0), (999, 0)); Day06.TurnOff ((499, 499), (500, 500)) ]

let () = Alcotest.run "2015-05" [ ("Day 05", [ ("can run day 05 examples", `Quick, test) ]) ]
