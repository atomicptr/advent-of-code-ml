open Alcotest

let check_ok func str expected =
  let ok = func str in
  check bool str expected ok

let test () =
  check_ok Day05.satisfies_rule_1 "aeiou" true;
  check_ok Day05.satisfies_rule_1 "aei" true;
  check_ok Day05.satisfies_rule_1 "xazegov" true;
  check_ok Day05.satisfies_rule_1 "aeiouaeiouaeiou" true;
  check_ok Day05.satisfies_rule_2 "xx" true;
  check_ok Day05.satisfies_rule_2 "abcdde" true;
  check_ok Day05.satisfies_rule_2 "aabbccdd" true;
  check_ok Day05.satisfies_rule_3 "aabbccdd" false;
  check_ok Day05.satisfies_rule_3 "xx" true;
  check_ok Day05.is_nice "ugknbfddgicrmopn" true;
  check_ok Day05.is_nice "aaa" true;
  check_ok Day05.is_nice "jchzalrnumimnmhp" false;
  check_ok Day05.is_nice "haegwjzuvuyypxyu" false;
  check_ok Day05.is_nice "dvszwmarrgswjxmb" false

let () = Alcotest.run "2015-05" [ ("Day 05", [ ("can run day 05 examples", `Quick, test) ]) ]
