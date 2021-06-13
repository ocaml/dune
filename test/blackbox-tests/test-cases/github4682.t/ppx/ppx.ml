let main () =
  let out = ref "" in
  let args =
    [ ("-o", Arg.Set_string out, "")
    ; ("--impl", Arg.Set_string (ref ""), "")
    ; ("--as-ppx", Arg.Set (ref false), "")
    ]
  in
  let anon _ = () in
  Arg.parse (Arg.align args) anon "";
  let out = open_out !out in
  close_out out;
