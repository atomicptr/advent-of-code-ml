type char_list = Quote | Char of char | Unicode of int

let parse str =
  let rec inner = function
    | [] -> []
    | '\\' :: 'x' :: num1 :: num2 :: rest ->
        Unicode (int_of_string ("0x" ^ String.make 1 num1 ^ String.make 1 num2)) :: inner rest
    | '\\' :: c :: rest -> Char c :: inner rest
    | '"' :: rest -> Quote :: inner rest
    | c :: rest -> Char c :: inner rest
  in
  inner (List.of_seq @@ String.to_seq str)

let rec to_string = function
  | [] -> ""
  | Quote :: rest -> "" ^ to_string rest
  | Char c :: rest -> String.make 1 c ^ to_string rest
  | Unicode u :: rest -> String.make 1 (Char.chr u) ^ to_string rest

let parse_to_string str = parse str |> to_string
let strlen str = parse_to_string str |> String.length
let total_length lst = lst |> List.map String.length |> List.fold_left ( + ) 0
let total_strlen lst = lst |> List.map strlen |> List.fold_left ( + ) 0

let run input =
  let lines = Base.split_on_newline input in
  print_endline ("Part 1 is " ^ string_of_int @@ (total_length lines - total_strlen lines))
