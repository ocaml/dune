open Import
open Build.O
open! No_io

let gen_rules sctx ~dir ~scope (t : Jbuild.Menhir.t) =
  let targets n = List.map ~f:(Path.relative dir) [n ^ ".ml"; n ^ ".mli"] in
  let flags =
    Super_context.expand_and_eval_set sctx ~scope ~dir t.flags
      ~standard:[] in
  let menhir =
    let menhir =
      Super_context.resolve_program sctx ~hint:"opam install menhir" "menhir" in
    fun ~extra_targets args->
      flags
      >>> (Build.run ~extra_targets
             menhir
             ~dir
             ~context:(Super_context.context sctx)
             args) in
  let add_rule_get_targets =
    Super_context.add_rule_get_targets sctx ~mode:t.mode ~loc:t.loc in
  let mly name = Path.relative dir (name ^ ".mly") in
  match t.merge_into with
  | None ->
    List.concat_map ~f:(fun name ->
      add_rule_get_targets (
        menhir
          ~extra_targets:(targets name)
          [ Dyn (fun x -> As x)
          ; Dep (mly name)]
      )) t.modules
  | Some merge_into ->
    add_rule_get_targets (
      menhir
        ~extra_targets:(targets merge_into)
        [ A "--base" ; A merge_into
        ; Dyn (fun x -> As x)
        ; Deps (List.map ~f:mly t.modules)
        ]
    )
