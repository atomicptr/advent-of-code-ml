open Alcotest

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

let check_gate input expected =
  let res = Day07.parse input in
  check string "check gate" expected (gate_to_string res)

let check_gate_result input wire expected =
  let res = Day07.eval input in
  check int ("check if wire: " ^ wire ^ " is " ^ string_of_int expected) expected (Day07.map_get wire res)

let check_example =
  check_gate_result
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

let test () =
  check_gate [ "1234"; "->"; "c" ] "1234 -> c";
  check_gate [ "a"; "AND"; "b"; "->"; "c" ] "a AND b -> c";
  check_gate [ "a"; "OR"; "b"; "->"; "c" ] "a OR b -> c";
  check_gate [ "a"; "LSHIFT"; "b"; "->"; "c" ] "a LSHIFT b -> c";
  check_gate [ "a"; "RSHIFT"; "b"; "->"; "c" ] "a RSHIFT b -> c";
  check_gate [ "NOT"; "b"; "->"; "c" ] "NOT b -> c";
  check_gate_result [ Day07.Set (Day07.Signal 123, Day07.Wire "x") ] "x" 123;
  check_gate_result [ Day07.And (Day07.Signal 7, Day07.Signal 7, Day07.Wire "res") ] "res" 7;

  check_example "d" 72;
  check_example "e" 507;
  check_example "f" 492;
  check_example "g" 114;
  check_example "h" 65412;
  check_example "i" 65079;
  check_example "x" 123;
  check_example "y" 456

let () = Alcotest.run "2015-07" [ ("Day 07", [ ("can run day 07 examples", `Quick, test) ]) ]
