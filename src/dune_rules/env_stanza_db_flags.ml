open Memo.O

let flags ~name ~root ~f =
  Env_stanza_db.inherited ~name ~root ~f:(fun ~parent ~dir local ->
    let* expander =
      let* context = Context.DB.by_dir dir in
      let* sctx = Super_context.find_exn (Context.name context) in
      Super_context.expander sctx ~dir
    in
    f ~parent expander local)
;;
