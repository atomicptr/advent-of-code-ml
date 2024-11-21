open CamelCase

let () =
  run
    [
      test "parse_to_string(\"\") = " (fun () -> StringValue.expect_equals "" (Day08.parse_to_string "\"\""));
      test "parse_to_string(\"abc\") = abc" (fun () ->
          StringValue.expect_equals "abc" (Day08.parse_to_string "\"abc\""));
      test "parse_to_string(\"aaa\"aaa\") = aaa\"aaa" (fun () ->
          StringValue.expect_equals "aaa\"aaa" (Day08.parse_to_string "\"aaa\\\"aaa"));
      test "parse_to_string(\"\\x27\") = '" (fun () ->
          StringValue.expect_equals "'" (Day08.parse_to_string "\"\\x27\""));
      test "strlen(\"\") = 0" (fun () -> IntValue.expect_equals 0 (Day08.strlen "\"\""));
      test "strlen(\"abc\") = 3" (fun () -> IntValue.expect_equals 3 (Day08.strlen "\"abc\""));
      test "strlen(\"aaa\"aaa\") = 7" (fun () -> IntValue.expect_equals 7 (Day08.strlen "\"aaa\\\"aaa"));
      test "strlen(\"\\x27\") = 1" (fun () -> IntValue.expect_equals 1 (Day08.strlen "\"\\x27\""));
      test "total length" (fun () ->
          IntValue.expect_equals 23 (Day08.total_length [ "\"\""; "\"abc\""; "\"aaa\\\"aaa\""; "\"\\x27\"" ]));
      test "total strlen" (fun () ->
          IntValue.expect_equals 11 (Day08.total_strlen [ "\"\""; "\"abc\""; "\"aaa\\\"aaa\""; "\"\\x27\"" ]));
    ]
