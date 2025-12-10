open Import

let cat =
  let info = Cmd.info "cat" in
  let term =
    let+ () = Term.const () in
    let bytes = Bytes.create (1 lsl 16) in
    Io.String_path.with_file_in ~binary:true "_build/trace.json" ~f:(fun i ->
      let rec loop () =
        match input i bytes 0 (Bytes.length bytes) with
        | 0 | (exception End_of_file) -> ()
        | n ->
          output stdout bytes 0 n;
          loop ()
      in
      loop ())
  in
  Cmd.v info term
;;

let group =
  let info =
    let doc = "Commands to view dune's event trace" in
    Cmd.info "trace" ~doc
  in
  Cmd.group info [ cat ]
;;
