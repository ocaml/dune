open Import

let gen_rules sctx ~dir ~scope (t : Jbuild.Menhir.t) =
  let targets n = List.map ~f:(Path.relative dir) [n ^ ".ml"; n ^ ".mli"] in
  let flags =
    List.map ~f:(Super_context.expand_vars sctx ~scope ~dir) t.flags in
  let menhir =
    let menhir =
      Super_context.resolve_program sctx ~hint:"opam install menhir" "menhir" in
    fun ~extra_targets ->
      Build.run ~extra_targets
        menhir
        ~dir
        ~context:(Super_context.context sctx) in
  let add_rule_get_targets =
    Super_context.add_rule_get_targets sctx ~mode:t.mode ~loc:t.loc in
  let mly name = Path.relative dir (name ^ ".mly") in
  match t.merge_into with
  | None ->
    List.concat_map ~f:(fun name ->
      add_rule_get_targets (
        menhir
          ~extra_targets:(targets name)
          [ As flags
          ; Dep (mly name)]
      )) t.modules
  | Some merge_into ->
    add_rule_get_targets (
      menhir
        ~extra_targets:(targets merge_into)
        [ A "--base" ; A merge_into
        ; As flags
        ; Deps (List.map ~f:mly t.modules)
        ]
    )
