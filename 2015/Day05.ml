let explode str = List.init (String.length str) (String.get str)

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
  test (explode str) 0

let satisfies_rule_2 str =
  let rec test lst =
    match lst with
    | [] | [ _ ] -> false
    | a :: b :: rest -> if a = b then true else test @@ (b :: rest)
  in
  test (explode str)

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
  test (explode str)

let is_nice str = satisfies_rule_1 str && satisfies_rule_2 str && satisfies_rule_3 str

let parse_file str =
  String.split_on_char '\n' str |> List.filter (fun s -> String.length s > 0) |> List.filter is_nice |> List.length

let run input = print_endline @@ "Part 1: " ^ string_of_int (parse_file input)
