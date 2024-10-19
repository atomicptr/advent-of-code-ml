let args = List.tl @@ Array.to_list Sys.argv

let get_day () = match args with
| day :: _ -> Some (int_of_string day)
| _ -> None
