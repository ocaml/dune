open Printf

let write_modules basedir num_modules =
  for current_mod = 1 to num_modules do
    let modname = sprintf "%s/m_%d" basedir current_mod in
    let f = open_out (sprintf "%s.ml" modname) in
    close_out f
  done
;;

let dune = {|
(library
 (name test))
|}

let write basedir =
  let () = Unix.mkdir basedir 0o777 in
  let f = open_out (Filename.concat basedir "dune") in
  output_string f dune;
  let () = close_out f in
  write_modules basedir
;;

let () =
  let basedir = ref "." in
  let num_modules = ref 0 in
  Arg.parse
    [ ( "-n"
      , Arg.Int (fun n -> num_modules := n)
      , "<n>  number of modules to include in the synthetic library" )
    ]
    (fun d -> basedir := d)
    (sprintf "usage: %s [basedir]" (Filename.basename Sys.argv.(0)));
  write !basedir !num_modules
;;
