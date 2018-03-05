open Import
open Build.O
open! No_io

module SC = Super_context

(* - [sctx]: super context. Stores all the informations about the
   current build context. The current compiler can be obtained via:

   {[
     (SC.context sctx).ocamlc
   ]}

   - [dir]: directory inside [_build/<context>/...] where the build happens.
   If the [(menhir ...)] appears in [src/jbuild], then [dir] is of the form
   [_build/<context>/src], for instance [_build/default/src].

   - [scope]: represent the scope this stanza is part of. Jbuilder allows to
   build multiple projects at once and splits the source tree into one
   scope per project

   - [t]: the parsed [(menhir ...)] stanza
*)
let gen_rules sctx ~dir ~scope (t : Jbuild.Menhir.t) =
  let targets n = List.map ~f:(Path.relative dir) [n ^ ".ml"; n ^ ".mli"] in
  (* This expands special variables such as ${ROOT} in the flags *)
  let flags = SC.expand_and_eval_set sctx ~scope ~dir t.flags ~standard:[] in
  let menhir_binary =
    SC.resolve_program sctx "menhir" ~hint:"opam install menhir"
  in
  (* [extra_targets] is to tell Jbuilder about generated files that do
     not appear in the menhir command line. *)
  let menhir ~extra_targets args =
    flags
    >>>
    Build.run ~extra_targets
      menhir_binary
      ~dir
      ~context:(SC.context sctx)
      args
  in
  let add_rule_get_targets =
    SC.add_rule_get_targets sctx ~mode:t.mode ~loc:t.loc
  in
  let mly name = Path.relative dir (name ^ ".mly") in
  match t.merge_into with
  | None ->
    List.concat_map t.modules ~f:(fun name ->
      add_rule_get_targets (
        menhir
          ~extra_targets:(targets name)
          [ Dyn (fun x -> As x)
          ; Dep (mly name)
          ]))
  | Some merge_into ->
    add_rule_get_targets (
      menhir
        ~extra_targets:(targets merge_into)
        [ A "--base" ; A merge_into
        ; Dyn (fun x -> As x)
        ; Deps (List.map ~f:mly t.modules)
        ]
    )
