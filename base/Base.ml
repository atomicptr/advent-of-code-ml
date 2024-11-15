let args = List.tl @@ Array.to_list Sys.argv

let get_day () =
  match args with
  | day :: _ -> Some (int_of_string day)
  | _ -> None

let explode str = List.init (String.length str) (String.get str)
let split_on_newline str = String.split_on_char '\n' str |> List.filter (fun s -> String.length s > 0)

let rec list_drop num lst =
  match (num, lst) with
  | 0, lst -> lst
  | _, [] -> []
  | n, _ :: rest -> list_drop (n - 1) rest

let rec list_take num lst =
  match (num, lst) with
  | 0, _ -> []
  | _, [] -> []
  | n, head :: rest -> head :: list_take (n - 1) rest

let list_slice index_from index_to lst = list_take (index_to - index_from + 1) (list_drop index_from lst)

(* From atomicptr/pathlib-ml *)

(** Read text from file *)
let read_text file =
  let in_channel = open_in_bin file in
  let str = really_input_string in_channel (in_channel_length in_channel) in
  close_in in_channel;
  str
