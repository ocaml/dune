let split_at_first_empty_line s =
  let len = String.length s in
  let rec loop i =
    if i + 1 >= len then
      (s, "")
    else if s.[i] = '\n' && s.[i + 1] = '\n' then
      let i = i + 1 in
      (String.sub s 0 i,
       String.sub s i (len - i))
    else
      loop (i + 1)
  in
  loop 0

let () =
  let ic = open_in_bin "README.org" in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  let oc = open_out_bin "README.org" in
  let before, after = split_at_first_empty_line s in
  Printf.fprintf oc "%s%%%%%,VERSION%%%%\n%s" before after;
  close_out oc
