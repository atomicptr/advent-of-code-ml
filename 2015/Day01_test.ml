open Alcotest

let check_code str expected = 
    let result = Day01.parse_and_eval str in check int str result expected

let check_entry_pos str expected =
    let result = Day01.parse_and_find_basement_entry_pos str in check int str result expected

let test_day_01 () =
    check_code "(())" 0;
    check_code "()()" 0;
    check_code "(((" 3;
    check_code "(()(()(" 3;
    check_code "())" (-1);
    check_code "))(" (-1);
    check_code ")))" (-3);
    check_code ")())())" (-3);
    check_entry_pos ")" 1;
    check_entry_pos "()())" 5

let () =
    Alcotest.run "2015-01" [ "Day 01", [ "can run day 01 examples", `Quick, test_day_01] ]
