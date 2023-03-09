open Printf

type lib =
  | Leaf
  | Internal

let dir_cols = 4

let count n = Array.to_list (Array.init n (fun k -> k + 1))

let write_directory base_dir dir_row dir_col =
  let mod_rows = 10 in
  let mod_cols = 10 in

  let dirname = sprintf "%s/dir_%d_%d" base_dir dir_row dir_col in
  Unix.mkdir dirname 0o777;

  for row = 1 to mod_rows do
    for col = 1 to mod_cols do
      let deps =
        if row = 1 then
          if dir_row = 1 then []
          else
            List.flatten
              (List.map
                 (fun k ->
                   List.map
                     (fun j ->
                       sprintf "M_%d_%d_%d_%d.f()" (dir_row - 1) j mod_rows k)
                     (count dir_cols))
                 (count mod_cols))
        else
          List.map
            (fun k -> sprintf "M_%d_%d_%d_%d.f()" dir_row dir_col (row - 1) k)
            (count mod_cols)
      in

      let deps = List.rev ("()" :: List.rev deps) in

      let str_deps = String.concat ";\n  " deps in
      let mod_text = sprintf "let f() =\n  %s\n" str_deps in
      let modname =
        sprintf "%s/m_%d_%d_%d_%d" dirname dir_row dir_col row col
      in
      let f = open_out (sprintf "%s.ml" modname) in
      output_string f mod_text;
      close_out f;
      let f = open_out (sprintf "%s.mli" modname) in
      output_string f "val f : unit -> unit";
      close_out f
    done
  done

let write_lib ~base_dir ~lib ~dune =
  let name =
    match lib with
    | Leaf -> "leaf"
    | Internal -> "internal"
  in
  let lib_dir = Filename.concat base_dir name in
  let () = Unix.mkdir lib_dir 0o777 in
  let f = open_out (Filename.concat lib_dir "dune") in
  output_string f dune;
  let () = close_out f in

  let row =
    match lib with
    | Leaf -> 2
    | Internal -> 1
  in
  for col = 1 to dir_cols do
    write_directory lib_dir row col
  done

let write base_dir =
  let () = Unix.mkdir base_dir 0o777 in

  let dune =
    {|
(include_subdirs unqualified)

(library
 (name leaf)
 (libraries internal))
|}
  in
  write_lib ~base_dir ~lib:Leaf ~dune;

  let dune =
    {|
(include_subdirs unqualified)

(library
 (name internal)
 (wrapped false))
|}
  in
  write_lib ~base_dir ~lib:Internal ~dune

let () =
  let base_dir = ref "." in
  Arg.parse []
    (fun d -> base_dir := d)
    (sprintf "usage: %s [base_dir]" (Filename.basename Sys.argv.(0)));

  write !base_dir
