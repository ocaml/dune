open Import
open Memo.O

let setup_ffi_rules ~sctx ~dir
    ({ loc; modules; library; flags } : Coqffi_stanza.t) =
  let* coqffi =
    Super_context.resolve_program ~dir sctx "coqffi" ~loc:(Some loc)
      ~hint:"opam install coq-coqffi"
  in
  let* lib = Coqffi_sources.lib ~dir ~library in
  let* modules =
    Coqffi_sources.modules_of ~loc ~lib ~modules
      ~ml_sources:(Dir_contents.get sctx ~dir >>= Dir_contents.ocaml)
  in
  let action =
    List.map modules ~f:(fun m ->
        let flags =
          Ordered_set_lang.eval flags ~eq:( = ) ~standard:[]
            ~parse:(fun ~loc:_ flag -> flag)
        in
        let args =
          [ Command.Args.Dep
              (Obj_dir.Module.cm_file_exn
                 (Lib_info.obj_dir (Lib.info lib))
                 ~kind:(Ocaml Cmi) m)
          ; A "-o"
          ; Target (Coqffi_sources.target_of ~dir (Module.name m))
          ; As flags
          ]
        in
        Command.run ~dir:(Path.build dir) coqffi args)
  in
  Super_context.add_rules ~loc ~dir sctx action
