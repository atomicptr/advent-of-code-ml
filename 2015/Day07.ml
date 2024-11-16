let max_int = 65535

type value = Signal of int | Wire of string

type gate =
  | Set of value * value
  | And of value * value * value
  | Or of value * value * value
  | Lshift of value * value * value
  | Rshift of value * value * value
  | Not of value * value

let parse_value v =
  match int_of_string_opt v with
  | Some v -> Signal v
  | None -> Wire v

let is_wire = function
  | Signal _ -> false
  | Wire _ -> true

let parse = function
  | [] -> failwith "Invalid input"
  | x :: "->" :: wire :: _ when is_wire (parse_value wire) -> Set (parse_value x, parse_value wire)
  | x :: "AND" :: y :: "->" :: wire :: _ when is_wire (parse_value wire) ->
      And (parse_value x, parse_value y, parse_value wire)
  | x :: "OR" :: y :: "->" :: wire :: _ when is_wire (parse_value wire) ->
      Or (parse_value x, parse_value y, parse_value wire)
  | x :: "LSHIFT" :: y :: "->" :: wire :: _ when is_wire (parse_value wire) ->
      Lshift (parse_value x, parse_value y, parse_value wire)
  | x :: "RSHIFT" :: y :: "->" :: wire :: _ when is_wire (parse_value wire) ->
      Rshift (parse_value x, parse_value y, parse_value wire)
  | "NOT" :: x :: "->" :: wire :: _ when is_wire (parse_value wire) -> Not (parse_value x, parse_value wire)
  | token :: _ -> failwith ("Unknown token: " ^ token)

let parse_string input = parse @@ String.split_on_char ' ' input

module StringMap = Map.Make (String)

let get_val map = function
  | Signal v -> v
  | Wire key -> (
      match StringMap.find_opt key map with
      | Some value -> value
      | None -> failwith @@ "Unknown wire: " ^ key)

let rec wires_are_set map = function
  | [] -> true
  | Signal _ :: rest -> wires_are_set map rest
  | Wire wire :: rest -> (
      match StringMap.find_opt wire map with
      | Some _ -> wires_are_set map rest
      | None -> false)

let eval lst =
  let rec intern map = function
    | [] -> map
    | Set (a, Wire wire) :: rest when wires_are_set map [ a ] -> intern (StringMap.add wire (get_val map a) map) rest
    | And (a, b, Wire wire) :: rest when wires_are_set map [ a; b ] ->
        intern (StringMap.add wire (get_val map a land get_val map b) map) rest
    | Or (a, b, Wire wire) :: rest when wires_are_set map [ a; b ] ->
        intern (StringMap.add wire (get_val map a lor get_val map b) map) rest
    | Lshift (a, b, Wire wire) :: rest when wires_are_set map [ a; b ] ->
        intern (StringMap.add wire (Int.shift_left (get_val map a) (get_val map b)) map) rest
    | Rshift (a, b, Wire wire) :: rest when wires_are_set map [ a; b ] ->
        intern (StringMap.add wire (Int.shift_right (get_val map a) (get_val map b)) map) rest
    | Not (a, Wire wire) :: rest when wires_are_set map [ a ] ->
        intern (StringMap.add wire (max_int - get_val map a) map) rest
    | g :: rest -> intern map (rest @ [ g ])
  in
  intern StringMap.empty lst

let map_get key map =
  match StringMap.find_opt key map with
  | Some v -> v
  | None -> failwith @@ "Unknown key: " ^ key

let eval_input input = Base.split_on_newline input |> List.map parse_string |> eval

let run input =
  let result = eval_input input in
  print_endline ("Part 1: a = " ^ string_of_int @@ map_get "a" result)
