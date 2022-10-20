let () = Printexc.record_backtrace true

let digest =
  let raw = Sys.getcwd () |> Filename.dirname |> Filename.basename in
  let idx =
    let store_path = Sys.getenv "DUNE_PWD_STORE" in
    match open_in store_path with
    | exception Sys_error _ ->
      let out = open_out_gen [ Open_append; Open_creat ] 0 store_path in
      Printf.fprintf out "%s\n" raw;
      close_out out;
      1
    | store ->
      let rec loop i =
        match input_line store with
        | line -> if line = raw then i else loop (i + 1)
        | exception End_of_file ->
          close_in store;
          let out = open_out_gen [ Open_append; Open_creat ] 0 store_path in
          Printf.fprintf out "%s\n" raw;
          close_out out;
          i
      in
      let index = loop 1 in
      close_in store;
      index
  in
  Printf.sprintf "$%d" idx

let () =
  print_endline "runnning...";
  let out = open_out "target" in
  output_string out "target";
  close_out out;
  Printf.printf "digest: %s\n" digest
