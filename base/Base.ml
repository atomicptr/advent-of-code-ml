let args = List.tl @@ Array.to_list Sys.argv

let get_day () = match args with
| day :: _ -> Some (int_of_string day)
| _ -> None

(* From atomicptr/pathlib-ml *)

(** Read text from file *)
let read_text file =
  let in_channel = open_in_bin file in
  let str = really_input_string in_channel (in_channel_length in_channel) in
  close_in in_channel;
  str
