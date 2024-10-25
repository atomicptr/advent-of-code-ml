let find_md5_suffix str start_pattern =
  let rec find_suffix str index =
    let hash = Digest.to_hex @@ Digest.string (str ^ string_of_int index) in
    if String.starts_with ~prefix:start_pattern hash then (index, hash) else find_suffix str (index + 1)
  in
  find_suffix str 1

let run input =
  let input = String.trim input in
  let index, hash = find_md5_suffix input "00000" in
  print_endline @@ "Part 1:\t" ^ input ^ "\t" ^ string_of_int index ^ "\t" ^ hash;
  let index, hash = find_md5_suffix input "000000" in
  print_endline @@ "Part 2:\t" ^ input ^ "\t" ^ string_of_int index ^ "\t" ^ hash
