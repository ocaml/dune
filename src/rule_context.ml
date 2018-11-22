open! Stdune
open Import

module Alias = Build_system.Alias

type t =
  { build_system : Build_system.t
  ; env : Env.t
  ; chdir : (Action.t, Action.t) Build.t
  ; context : Context.t
  ; file_tree : File_tree.t
  }

let context t = t.context

let make ~build_system ~env ~chdir ~context ~file_tree =
  { build_system
  ; env
  ; chdir
  ; context
  ; file_tree
  }

let add_rule t ?sandbox ?mode ?locks ?loc build =
  let build = Build.O.(>>>) build t.chdir in
  Build_system.add_rule t.build_system
    (Build_interpret.Rule.make ?sandbox ?mode ?locks ?loc
       ~context:(Some t.context) ~env:(Some t.env) build)

let add_rule_get_targets t ?sandbox ?mode ?locks ?loc build =
  let build = Build.O.(>>>) build t.chdir in
  let rule =
    Build_interpret.Rule.make ?sandbox ?mode ?locks ?loc
      ~context:(Some t.context) ~env:(Some t.env) build
  in
  Build_system.add_rule t.build_system rule;
  List.map rule.targets ~f:Build_interpret.Target.path

let add_rules t ?sandbox builds =
  List.iter builds ~f:(add_rule t ?sandbox)

let add_alias_deps t alias ?dyn_deps deps =
  Alias.add_deps t.build_system alias ?dyn_deps deps

let add_alias_action t alias ~loc ?locks ~stamp action =
  Alias.add_action t.build_system ~context:t.context ~env:(Some t.env)
    alias ~loc ?locks ~stamp action

let prefix_rules t prefix ~f =
  Build_system.prefix_rules t.build_system prefix ~f

module Deps = struct
  open Build.O
  open Dune_file.Dep_conf

  let make_alias expander s =
    let loc = String_with_vars.loc s in
    Expander.expand_path expander s
    |> Alias.of_user_written_path ~loc

  let dep t expander = function
    | File s ->
      let path = Expander.expand_path expander s in
      Build.path path
      >>^ fun () -> [path]
    | Alias s ->
      Alias.dep (make_alias expander s)
      >>^ fun () -> []
    | Alias_rec s ->
      Alias.dep_rec ~loc:(String_with_vars.loc s) ~file_tree:t.file_tree
        (make_alias expander s)
      >>^ fun () -> []
    | Glob_files s -> begin
        let loc = String_with_vars.loc s in
        let path = Expander.expand_path expander s in
        match Glob_lexer.parse_string (Path.basename path) with
        | Ok re ->
          let dir = Path.parent_exn path in
          Build.paths_glob ~loc ~dir (Re.compile re)
          >>^ Path.Set.to_list
        | Error (_pos, msg) ->
          Errors.fail (String_with_vars.loc s) "invalid glob: %s" msg
      end
    | Source_tree s ->
      let path = Expander.expand_path expander s in
      Build.source_tree ~dir:path ~file_tree:t.file_tree
      >>^ Path.Set.to_list
    | Package p ->
      let pkg = Package.Name.of_string (Expander.expand_str expander p) in
      Alias.dep (Alias.package_install ~context:t.context ~pkg)
      >>^ fun () -> []
    | Universe ->
      Build.path Build_system.universe_file
      >>^ fun () -> []
    | Env_var var_sw ->
      let var = Expander.expand_str expander var_sw in
      Build.env_var var
      >>^ fun () -> []

  let make_interpreter ~f t ~expander l =
    let forms = Expander.Resolved_forms.empty () in
    let expander =
      Expander.with_record_no_ddeps expander forms
        ~dep_kind:Optional ~map_exe:(fun x -> x)
    in
    let deps =
      List.map l ~f:(f t expander)
      |> Build.all
      >>^ List.concat in
    Build.fanout4
      deps
      (Build.record_lib_deps (Expander.Resolved_forms.lib_deps forms))
      (let ddeps = String.Map.to_list (Expander.Resolved_forms.ddeps forms) in
       Build.all (List.map ddeps ~f:snd))
      (Build.path_set (Expander.Resolved_forms.sdeps forms))
    >>^ (fun (deps, _, _, _) -> deps)

  let interpret = make_interpreter ~f:dep

  let interpret_named =
    make_interpreter ~f:(fun t expander -> function
      | Bindings.Unnamed p ->
        dep t expander p >>^ fun l ->
        List.map l ~f:(fun x -> Bindings.Unnamed x)
      | Named (s, ps) ->
        Build.all (List.map ps ~f:(dep t expander)) >>^ fun l ->
        [Bindings.Named (s, List.concat l)])
end
