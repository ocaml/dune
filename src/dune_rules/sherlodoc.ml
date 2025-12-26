open Import
open Memo.O

module Paths = struct
  let db_dot_js ~dir = Path.Build.relative dir "db.js"
  let sherlodoc_dot_js ~dir = Path.Build.relative dir "sherlodoc.js"
  let db_marshal ~dir = Path.Build.relative dir "db.marshal"
end

let resolve_sherlodoc sctx ~dir =
  Super_context.resolve_program sctx ~dir "sherlodoc" ~loc:None
;;

let add_index_db_rule sctx ~dir ~external_odocls odocls =
  let program = resolve_sherlodoc sctx ~dir in
  let action =
    Command.run_dyn_prog
      ~dir:(Path.build dir)
      program
      Command.Args.
        [ A "index"
        ; Deps (List.map ~f:Path.build external_odocls)
        ; S (List.map ~f:(fun ext -> S [ A "--favoured"; Dep (Path.build ext) ]) odocls)
        ; A "--favoured-prefixes"
        ; A {|""|}
        ; A "--format=js"
        ; A "--db"
        ; Target (Paths.db_dot_js ~dir)
        ]
  in
  Super_context.add_rule sctx ~dir action
;;

let add_index_marshal_rule sctx ~dir ~external_odocls odocls =
  let program = resolve_sherlodoc sctx ~dir in
  let action =
    Command.run_dyn_prog
      ~dir:(Path.build dir)
      program
      Command.Args.
        [ A "index"
        ; Deps (List.map ~f:Path.build external_odocls)
        ; S (List.map ~f:(fun ext -> S [ A "--favoured"; Dep (Path.build ext) ]) odocls)
        ; A "--favoured-prefixes"
        ; A {|""|}
        ; A "--format=marshal"
        ; A "--db"
        ; Target (Paths.db_marshal ~dir)
        ]
  in
  Super_context.add_rule sctx ~dir action
;;

let sherlodoc_dot_js sctx ~dir =
  let program = resolve_sherlodoc sctx ~dir in
  Super_context.add_rule
    ~dir
    sctx
    (Command.run_dyn_prog
       ~dir:(Path.build dir)
       program
       [ A "js"; Target (Paths.sherlodoc_dot_js ~dir) ])
;;

let is_installed sctx ~dir =
  Action_builder.map ~f:Result.is_ok (resolve_sherlodoc sctx ~dir)
;;

let odoc_args sctx ~search_db ~dir_sherlodoc_dot_js ~html_root =
  Command.Args.Dyn
    (let open Action_builder.O in
     let+ is_installed = is_installed sctx ~dir:dir_sherlodoc_dot_js in
     if is_installed
     then (
       let sherlodoc_js =
         Path.build @@ Paths.sherlodoc_dot_js ~dir:dir_sherlodoc_dot_js
       in
       (* Compute relative paths from html_root (the -o directory).
          odoc will then compute relative paths from each HTML file to these URIs. *)
       let search_db_uri =
         Path.reach (Path.build search_db) ~from:(Path.build html_root)
       in
       let sherlodoc_js_uri = Path.reach sherlodoc_js ~from:(Path.build html_root) in
       Command.Args.S
         [ Hidden_deps (Dep.Set.of_files [ Path.build search_db; sherlodoc_js ])
         ; A "--search-uri"
         ; A search_db_uri
         ; A "--search-uri"
         ; A sherlodoc_js_uri
         ])
     else Command.Args.empty)
;;

let search_db sctx ~dir ~external_odocls odocls =
  let+ () = add_index_db_rule sctx ~dir ~external_odocls odocls in
  Paths.db_dot_js ~dir
;;

let search_db_marshal sctx ~dir ~external_odocls odocls =
  let+ () = add_index_marshal_rule sctx ~dir ~external_odocls odocls in
  Paths.db_marshal ~dir
;;
