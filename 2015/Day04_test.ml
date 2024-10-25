open Alcotest

let check_mine_hash key expected =
  let index, _hash = Day04.find_md5_suffix key "00000" in
  check int key expected index

let test () =
  check_mine_hash "abcdef" 609043;
  check_mine_hash "pqrstuv" 1048970

let () = Alcotest.run "2015-04" [ ("Day 04", [ ("can run day 04 examples", `Quick, test) ]) ]
