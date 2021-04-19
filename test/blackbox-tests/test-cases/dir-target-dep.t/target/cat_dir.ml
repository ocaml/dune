let dir = Sys.argv.(1)

let write f =
  let path = Filename.concat dir f in
  let out = open_out path in
  output_string out (f ^ " contents\n");
  close_out_noerr out

let input_all t =
  let buffer = Buffer.create 0 in
  let rec loop () =
    Buffer.add_channel buffer t 65536;
    loop ()
  in
  try loop () with
  | End_of_file -> Buffer.contents buffer

let () =
  let files =
    Sys.readdir dir |> Array.to_list |> ListLabels.sort ~cmp:String.compare
  in
  ListLabels.iter files ~f:(fun file ->
    let inp = open_in (Filename.concat dir file) in
    Printf.printf "%s:\n%s\n" file (input_all inp)
  )
