open Import
open Memo.O

module Paths = struct
  let db_dot_js ~dir = Path.Build.relative dir "db.js"
  let sherlodoc_dot_js ~dir = Path.Build.relative dir "sherlodoc.js"
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

let odoc_args sctx ~search_db ~dir_sherlodoc_dot_js =
  Command.Args.Dyn
    (let open Action_builder.O in
     let+ is_installed = is_installed sctx ~dir:dir_sherlodoc_dot_js in
     if is_installed
     then (
       let sherlodoc_js =
         Path.build @@ Paths.sherlodoc_dot_js ~dir:dir_sherlodoc_dot_js
       in
       Command.Args.S
         [ A "--search-uri"
         ; Dep (Path.build search_db)
         ; A "--search-uri"
         ; Dep sherlodoc_js
         ])
     else Command.Args.empty)
;;

let search_db sctx ~dir ~external_odocls odocls =
  let+ () = add_index_db_rule sctx ~dir ~external_odocls odocls in
  Paths.db_dot_js ~dir
;;
