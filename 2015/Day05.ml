let satisfies_rule_1 str =
  let is_vowel = function
    | 'a' | 'e' | 'i' | 'o' | 'u' -> true
    | _ -> false
  in
  let rec test lst vowel_counter =
    match lst with
    | [] -> vowel_counter >= 3
    | c :: rest -> test rest (if is_vowel c then vowel_counter + 1 else vowel_counter)
  in
  test (Base.explode str) 0

let satisfies_rule_2 str =
  let rec test lst =
    match lst with
    | [] | [ _ ] -> false
    | a :: b :: rest -> if a = b then true else test @@ (b :: rest)
  in
  test (Base.explode str)

let satisfies_rule_3 str =
  let rec test lst =
    match lst with
    | [] | [ _ ] -> true
    | 'a' :: 'b' :: _ -> false
    | 'c' :: 'd' :: _ -> false
    | 'p' :: 'q' :: _ -> false
    | 'x' :: 'y' :: _ -> false
    | _ :: tl -> test tl
  in
  test (Base.explode str)

let is_nice str = satisfies_rule_1 str && satisfies_rule_2 str && satisfies_rule_3 str

module StringMap = Map.Make (String)

let satisfies_new_rule_1 str =
  let increment map key =
    StringMap.update key
      (function
        | Some value -> Some (value + 1)
        | None -> Some 1)
      map
  in
  let rec parse lst map =
    match lst with
    | [] | [ _ ] -> map
    | a :: b :: c :: d :: rest when a == b && b == c && c == d ->
        parse (b :: c :: d :: rest) (increment map (String.make 1 a ^ String.make 1 b))
    | a :: b :: c :: rest when a == b && b == c -> parse (b :: c :: rest) map
    | a :: b :: rest -> parse (b :: rest) (increment map (String.make 1 a ^ String.make 1 b))
  in
  let result = List.filter (fun (_, res) -> res >= 2) (StringMap.to_list @@ parse (Base.explode str) StringMap.empty) in
  List.length result > 0

let satisfies_new_rule_2 str =
  let rec test = function
    | [] | [ _ ] | [ _; _ ] -> false
    | a :: b :: c :: rest -> if a == c then true else test @@ (b :: c :: rest)
  in
  test (Base.explode str)

let is_nice_part_2 str = satisfies_new_rule_1 str && satisfies_new_rule_2 str
let parse_file str func = Base.split_on_newline str |> List.filter func |> List.length

let run input =
  print_endline @@ "Part 1: " ^ string_of_int (parse_file input is_nice);
  print_endline @@ "Part 2: " ^ string_of_int (parse_file input is_nice_part_2)
