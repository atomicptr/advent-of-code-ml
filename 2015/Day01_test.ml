open CamelCase

let check_code expected input = IntValue.expect_equals expected (Day01.parse_and_eval input)
let check_entry_pos expected input = IntValue.expect_equals expected (Day01.parse_and_find_basement_entry_pos input)

let () =
  run
    [
      test "check code: (())" (fun () -> check_code 0 "(())");
      test "check code: ()()" (fun () -> check_code 0 "()()");
      test "check code: (((" (fun () -> check_code 3 "(((");
      test "check code: (()(()(" (fun () -> check_code 3 "(()(()(");
      test "check code: ())" (fun () -> check_code (-1) "())");
      test "check code: ))(" (fun () -> check_code (-1) "))(");
      test "check code: )))" (fun () -> check_code (-3) ")))");
      test "check code: )())())" (fun () -> check_code (-3) ")())())");
      test "check entry pos for: )" (fun () -> check_entry_pos 1 ")");
      test "check entry pos for: ()())" (fun () -> check_entry_pos 5 "()())");
    ]
