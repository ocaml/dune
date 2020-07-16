open Printf

let process_line =
  let path_re = Str.regexp {|^\([SB]\) /.+/lib/\(.+\)$|} in
  let ppx_re = Str.regexp {|^FLG -ppx '/.+/\.ppx/\(.+\)$|} in
  let special_pp_re =
    Str.regexp {|^FLG -pp '/.+/_build/install/default/bin/\(.+\)$|}
  in
  fun line ->
    line
    |> Str.replace_first path_re {|\1 $LIB_PREFIX/lib/\2|}
    |> Str.global_replace ppx_re {|FLG -ppx '$PPX/\1|}
    |> Str.global_replace special_pp_re {|FLG -pp '$BIN/\1|}

let () =
  let files = ref [] in
  let anon s = files := s :: !files in
  let usage = sprintf "%s [FILES]" (Filename.basename Sys.executable_name) in
  Arg.parse [] anon usage;
  let lines = ref [] in
  List.iter
    (fun f ->
      Printf.printf "# Processing %s\n" f;
      let ch = open_in f in
      let rec loop () =
        match input_line ch with
        | exception End_of_file -> ()
        | line ->
            lines := process_line line :: !lines;
            loop ()
      in
      loop ();
      !lines |> List.sort_uniq compare |> List.iter (fun s -> print_endline s);
      close_in ch)
    !files
