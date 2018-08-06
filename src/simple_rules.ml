open Import
open Jbuild
open Build.O
open! No_io

module SC = Super_context

let interpret_locks sctx ~dir ~scope locks =
  List.map locks ~f:(SC.expand_vars_path sctx ~dir ~scope)

let user_rule sctx ~dir ~scope (rule : Rule.t) =
  let targets : SC.Action.targets =
    match rule.targets with
    | Infer -> Infer
    | Static fns -> Static (List.map fns ~f:(Path.relative dir))
  in
  SC.add_rule_get_targets sctx ~mode:rule.mode ~loc:rule.loc
    ~locks:(interpret_locks sctx ~dir ~scope rule.locks)
    (SC.Deps.interpret_named sctx ~scope ~dir rule.deps
     >>>
     SC.Action.run
       sctx
       (snd rule.action)
       ~loc:(fst rule.action)
       ~dir
       ~bindings:(Pform.Map.of_bindings rule.deps)
       ~dep_kind:Required
       ~targets
       ~targets_dir:dir
       ~scope)

let copy_files sctx ~dir ~scope ~src_dir (def: Copy_files.t) =
  let loc = String_with_vars.loc def.glob in
  let glob_in_src =
    let src_glob = SC.expand_vars_string sctx ~dir def.glob ~scope in
    Path.relative src_dir src_glob ~error_loc:loc
  in
  (* The following condition is required for merlin to work.
     Additionally, the order in which the rules are evaluated only
     ensures that [sources_and_targets_known_so_far] returns the
     right answer for sub-directories only. *)
  if not (Path.is_descendant glob_in_src ~of_:src_dir) then
    Loc.fail loc "%s is not a sub-directory of %s"
      (Path.to_string_maybe_quoted glob_in_src) (Path.to_string_maybe_quoted src_dir);
  let glob = Path.basename glob_in_src in
  let src_in_src = Path.parent_exn glob_in_src in
  let re =
    match Glob_lexer.parse_string glob with
    | Ok re ->
      Re.compile re
    | Error (_pos, msg) ->
      Loc.fail (String_with_vars.loc def.glob) "invalid glob: %s" msg
  in
  (* add rules *)
  let src_in_build = Path.append (SC.context sctx).build_dir src_in_src in
  let files = SC.eval_glob sctx ~dir:src_in_build re in
  List.map files ~f:(fun basename ->
    let file_src = Path.relative src_in_build basename in
    let file_dst = Path.relative dir basename in
    SC.add_rule sctx
      ((if def.add_line_directive
        then Build.copy_and_add_line_directive
        else Build.copy)
         ~src:file_src
         ~dst:file_dst);
    file_dst)

let add_alias sctx ~dir ~name ~stamp ~loc ?(locks=[]) build =
  let alias = Build_system.Alias.make name ~dir in
  SC.add_alias_action sctx alias ~loc ~locks ~stamp build

let alias sctx ~dir ~scope (alias_conf : Alias_conf.t) =
  let enabled =
    match alias_conf.enabled_if with
    | None -> true
    | Some blang ->
      let f : String_with_vars.t Blang.expander =
        { f = fun ~mode sw ->
            ( String_with_vars.loc sw
            , Super_context.expand_vars sctx ~scope ~mode ~dir sw
            )
        } in
      Blang.eval_bool blang ~dir ~f
  in
  let stamp =
    let module S = Sexp.To_sexp in
    Sexp.List
      [ Sexp.unsafe_atom_of_string "user-alias"
      ; Jbuild.Bindings.sexp_of_t Jbuild.Dep_conf.sexp_of_t alias_conf.deps
      ; S.option Action.Unexpanded.sexp_of_t
          (Option.map alias_conf.action ~f:snd)
      ]
  in
  let loc = Some alias_conf.loc in
  if enabled then
    add_alias sctx
      ~dir
      ~loc
      ~name:alias_conf.name
      ~stamp
      ~locks:(interpret_locks sctx ~dir ~scope alias_conf.locks)
      (SC.Deps.interpret_named sctx ~scope ~dir alias_conf.deps
       >>>
       match alias_conf.action with
       | None -> Build.progn []
       | Some (loc, action) ->
         SC.Action.run
           sctx
           action
           ~loc
           ~dir
           ~dep_kind:Required
           ~bindings:(Pform.Map.of_bindings alias_conf.deps)
           ~targets:Alias
           ~targets_dir:dir
           ~scope)
  else
    add_alias sctx
      ~loc
      ~dir
      ~name:alias_conf.name
      ~stamp
      (Build.return (Action.Progn []))
