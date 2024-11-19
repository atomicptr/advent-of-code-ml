open CamelCase

let value_to_string = function
  | Day07.Signal n -> string_of_int n
  | Day07.Wire s -> s

let gate_to_string = function
  | Day07.Set (a, var) -> value_to_string a ^ " -> " ^ value_to_string var
  | Day07.And (a, b, var) -> value_to_string a ^ " AND " ^ value_to_string b ^ " -> " ^ value_to_string var
  | Day07.Or (a, b, var) -> value_to_string a ^ " OR " ^ value_to_string b ^ " -> " ^ value_to_string var
  | Day07.Lshift (a, b, var) -> value_to_string a ^ " LSHIFT " ^ value_to_string b ^ " -> " ^ value_to_string var
  | Day07.Rshift (a, b, var) -> value_to_string a ^ " RSHIFT " ^ value_to_string b ^ " -> " ^ value_to_string var
  | Day07.Not (a, var) -> "NOT " ^ value_to_string a ^ " -> " ^ value_to_string var

let check_parsing expected input = StringValue.expect_equals expected (Day07.parse input |> gate_to_string)
let check_gate_result expected input wire = IntValue.expect_equals expected (Day07.eval input |> Day07.map_get wire)

let check_example expected =
  check_gate_result expected
    [
      Day07.Set (Day07.Signal 123, Day07.Wire "x");
      Day07.Set (Day07.Signal 456, Day07.Wire "y");
      Day07.And (Day07.Wire "x", Day07.Wire "y", Day07.Wire "d");
      Day07.Or (Day07.Wire "x", Day07.Wire "y", Day07.Wire "e");
      Day07.Lshift (Day07.Wire "x", Day07.Signal 2, Day07.Wire "f");
      Day07.Rshift (Day07.Wire "y", Day07.Signal 2, Day07.Wire "g");
      Day07.Not (Day07.Wire "x", Day07.Wire "h");
      Day07.Not (Day07.Wire "y", Day07.Wire "i");
    ]

let () =
  run
    [
      test "check parsing: 1234 -> c" (fun () -> check_parsing "1234 -> c" [ "1234"; "->"; "c" ]);
      test "check parsing: a AND b -> c" (fun () -> check_parsing "a AND b -> c" [ "a"; "AND"; "b"; "->"; "c" ]);
      test "check parsing: a OR b -> c" (fun () -> check_parsing "a OR b -> c" [ "a"; "OR"; "b"; "->"; "c" ]);
      test "check parsing: a LSHIFT b -> c" (fun () ->
          check_parsing "a LSHIFT b -> c" [ "a"; "LSHIFT"; "b"; "->"; "c" ]);
      test "check parsing: a RSHIFT b -> c" (fun () ->
          check_parsing "a RSHIFT b -> c" [ "a"; "RSHIFT"; "b"; "->"; "c" ]);
      test "check parsing: NOT b -> c" (fun () -> check_parsing "NOT b -> c" [ "NOT"; "b"; "->"; "c" ]);
      test "check eval: 123 -> x" (fun () -> check_gate_result 123 [ Day07.Set (Day07.Signal 123, Day07.Wire "x") ] "x");
      test "check eval: 7 AND 7 -> res" (fun () ->
          check_gate_result 7 [ Day07.And (Day07.Signal 7, Day07.Signal 7, Day07.Wire "res") ] "res");
      test "check example value: d = 72" (fun () -> check_example 72 "d");
      test "check example value: e = 507" (fun () -> check_example 507 "e");
      test "check example value: f = 492" (fun () -> check_example 492 "f");
      test "check example value: g = 114" (fun () -> check_example 114 "g");
      test "check example value: h = 65412" (fun () -> check_example 65412 "h");
      test "check example value: i = 65079" (fun () -> check_example 65079 "i");
      test "check example value: x = 123" (fun () -> check_example 123 "x");
      test "check example value: y = 456" (fun () -> check_example 456 "y");
    ]
