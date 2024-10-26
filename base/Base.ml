let args = List.tl @@ Array.to_list Sys.argv

let get_day () =
  match args with
  | day :: _ -> Some (int_of_string day)
  | _ -> None

let explode str = List.init (String.length str) (String.get str)
let split_on_newline str = String.split_on_char '\n' str |> List.filter (fun s -> String.length s > 0)

(* From atomicptr/pathlib-ml *)

(** Read text from file *)
let read_text file =
  let in_channel = open_in_bin file in
  let str = really_input_string in_channel (in_channel_length in_channel) in
  close_in in_channel;
  str
