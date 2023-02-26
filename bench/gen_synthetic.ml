open Printf

let dir_rows = ref 1

let dir_cols = ref 1

let mod_rows = ref 1

let mod_cols = ref 1

let comment_size = ref 5000

let count n = Array.to_list (Array.init n (fun k -> k + 1))

let write_directory basedir dir_row dir_col =
  let dirname = sprintf "%s/dir_%d_%d" basedir dir_row dir_col in
  Unix.mkdir dirname 0o777;

  for row = 1 to !mod_rows do
    for col = 1 to !mod_cols do
      let deps =
        if row = 1 then
          if dir_row = 1 then []
          else
            List.flatten
              (List.map
                 (fun k ->
                   List.map
                     (fun j ->
                       sprintf "M_%d_%d_%d_%d.f()" (dir_row - 1) j !mod_rows k)
                     (count !dir_cols))
                 (count !mod_cols))
        else
          List.map
            (fun k -> sprintf "M_%d_%d_%d_%d.f()" dir_row dir_col (row - 1) k)
            (count !mod_cols)
      in

      let deps = List.rev ("()" :: List.rev deps) in

      let str_deps = String.concat ";\n  " deps in
      let comment = String.make !comment_size 'X' in
      let mod_text = sprintf "(* %s *)\nlet f() =\n  %s\n" comment str_deps in
      let modname =
        sprintf "%s/m_%d_%d_%d_%d" dirname dir_row dir_col row col
      in
      let f = open_out (sprintf "%s.ml" modname) in
      output_string f mod_text;
      close_out f;
      let f = open_out (sprintf "%s.mli" modname) in
      output_string f "val f : unit -> unit ";
      close_out f
    done
  done;
  let f = open_out (sprintf "%s/main.ml" basedir) in
  let modname =
    sprintf "Test.M_%d_%d_%d_%d" dir_row dir_col !mod_rows !mod_cols
  in
  let main = sprintf "let () = %s.f()" modname in
  output_string f main;
  close_out f

let dune =
  {|
(include_subdirs unqualified)

(library
 (name test)
 (modules :standard \ main))

(executable
 (name main)
 (libraries test)
 (modules main))
|}

let write basedir =
  let () = Unix.mkdir basedir 0o777 in
  let f = open_out (Filename.concat basedir "dune") in
  output_string f dune;
  let () = close_out f in

  let basedir = Filename.concat basedir "src" in
  let () = Unix.mkdir basedir 0o777 in
  for row = 1 to !dir_rows do
    for col = 1 to !dir_cols do
      write_directory basedir row col
    done
  done

let () =
  let basedir = ref "." in
  Arg.parse
    [ ( "-n"
      , Arg.Int
          (fun n ->
            dir_rows := n;
            dir_cols := n;
            mod_rows := n;
            mod_cols := n)
      , "<n>  set all of -dir-rows, -dir-cols, -mod-rows, -mod-cols to the \
         same value" )
    ; ("-row", Arg.Int (fun n -> dir_rows := n), "<n> set row")
    ; ("-col", Arg.Int (fun n -> dir_cols := n), "<n> set col")
    ; ("-mrow", Arg.Int (fun n -> mod_rows := n), "<n> set mod-row")
    ; ("-mcol", Arg.Int (fun n -> mod_cols := n), "<n> set mod-col")
    ]
    (fun d -> basedir := d)
    (sprintf "usage: %s [basedir]" (Filename.basename Sys.argv.(0)));

  write !basedir
