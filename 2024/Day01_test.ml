open CamelCase

let example_input = {|3   4
4   3
2   5
1   3
3   9
3   3|}

let () =
  run ~title:__FILE__
    [
      test "find min [1; 2; 3; 4]" (fun () ->
          let min, other = Day01.find_min [ 1; 2; 3; 4 ] in
          expect_some min
          >> IntValue.expect_equals 1 (Option.get min)
          >> IntValue.expect_equals 3 (List.length other)
          >> expect_some (List.find_opt (( = ) 2) other)
          >> expect_some (List.find_opt (( = ) 3) other)
          >> expect_some (List.find_opt (( = ) 4) other));
      test "total distance of example" (fun () ->
          IntValue.expect_equals 11 (Day01.total_distance [ 3; 4; 2; 1; 3; 3 ] [ 4; 3; 5; 3; 9; 3 ]));
      test "parse line \"3   4\"" (fun () ->
          let elem1, elem2 = Day01.parse_line "3   4" in
          IntValue.expect_equals 3 elem1 >> IntValue.expect_equals 4 elem2);
      test "parse input of example" (fun () ->
          let lst1, lst2 = Day01.parse_input example_input in
          IntValue.expect_equals 11 (Day01.total_distance lst1 lst2));
      test "count occurances 3 should be 3 times in: [4; 3; 5; 3; 9; 3]" (fun () ->
          IntValue.expect_equals 3 (Day01.count_occurances 3 [ 4; 3; 5; 3; 9; 3 ]));
      test "calculate similarity score for example" (fun () ->
          IntValue.expect_equals 31 (Day01.calculate_similarity_score [ 3; 4; 2; 1; 3; 3 ] [ 4; 3; 5; 3; 9; 3 ]));
    ]
