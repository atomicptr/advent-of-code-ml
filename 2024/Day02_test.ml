open CamelCase

let example_input = {|7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9|}

let () =
  run ~title:__FILE__
    [
      test "example: 7 6 4 2 1 -> SAFE" (fun () -> expect_true @@ Day02.is_report_safe [ 7; 6; 4; 2; 1 ]);
      test "example: 1 2 7 8 9 -> UNSAFE" (fun () -> expect_false @@ Day02.is_report_safe [ 1; 2; 7; 8; 9 ]);
      test "example: 9 7 6 2 1 -> UNSAFE" (fun () -> expect_false @@ Day02.is_report_safe [ 1; 3; 2; 4; 5 ]);
      test "example: 1 3 2 4 5 -> UNSAFE" (fun () -> expect_false @@ Day02.is_report_safe [ 1; 3; 2; 4; 5 ]);
      test "example: 8 6 4 4 1 -> UNSAFE" (fun () -> expect_false @@ Day02.is_report_safe [ 8; 6; 4; 4; 1 ]);
      test "example: 1 3 6 7 9 -> SAFE" (fun () -> expect_true @@ Day02.is_report_safe [ 1; 3; 6; 7; 9 ]);
      test "example has 2x safe reports" (fun () ->
          IntValue.expect_equals 2
            (Day02.parse_data example_input
            |> List.map Day02.is_report_safe
            |> List.fold_left (fun acc item -> if item then acc + 1 else acc) 0));
    ]
