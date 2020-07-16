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
  let files = Sys.argv |> Array.to_list |> List.tl |> List.sort compare in
  List.iter
    (fun f ->
      printf "# Processing %s\n" f;
      let ch = open_in f in
      let rec all_lines lines =
        match input_line ch with
        | exception End_of_file -> lines
        | line -> all_lines (process_line line :: lines)
      in
      all_lines [] |> List.sort compare |> List.iter print_endline;
      close_in ch)
    files
