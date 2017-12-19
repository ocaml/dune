
let () =
  let fname = Sys.argv.(1) in
  if Filename.check_suffix fname ".re"
  || Filename.check_suffix fname ".rei" then (
    let ch = open_in fname in
    let rec loop () =
      match input_line ch with
      | exception End_of_file -> ()
      | line -> print_endline line; loop () in
    loop ();
    close_in ch
  ) else (
    Format.eprintf "%s is not a reason source@.%!" fname;
    exit 1
  )
