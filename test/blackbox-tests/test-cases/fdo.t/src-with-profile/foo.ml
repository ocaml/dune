print_string "<root>"
;;

let sep = Filename.dir_sep in
assert ((String.length sep) = 1);
let l = Sys.executable_name
        |> String.split_on_char sep.[0] in
(* print the last 4 elements if exist *)
let len = (List.length l) in
List.iteri
  (fun i dir ->
     if ((len - i) < 5) then Printf.printf "/%s" dir)
  l
;;

print_endline ": hello from fdo!"
;;
