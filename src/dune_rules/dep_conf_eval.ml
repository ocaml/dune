open Import
open Action_builder.O

let make_sandboxing_config config =
  let loc = Dep_conf.Sandbox_config.loc config in
  Dep_conf.Sandbox_config.fold config ~init:[] ~f:(fun kind acc ->
    let partial =
      match kind with
      | `None -> Sandbox_config.Partial.no_sandboxing
      | `Always -> Sandbox_config.Partial.needs_sandboxing
      | `Preserve_file_kind -> Sandbox_config.Partial.disallow Sandbox_mode.symlink
    in
    partial :: acc)
  |> Dune_engine.Sandbox_config.Partial.merge ~loc
;;

let make_alias expander s =
  let loc = String_with_vars.loc s in
  Expander.expand_path expander s >>| Alias.of_user_written_path ~loc
;;

let package_install ~(context : Build_context.t) ~(pkg : Package.t) =
  let dir =
    let dir = Package.dir pkg in
    Path.Build.append_source context.build_dir dir
  in
  let name = Package.name pkg in
  sprintf ".%s-files" (Package.Name.to_string name)
  |> Alias.Name.of_string
  |> Alias.make ~dir
;;

type dep_evaluation_result =
  | Simple of Path.t list Memo.t
  | Other of Path.t list Action_builder.t

let to_action_builder = function
  | Simple paths ->
    let* paths = Action_builder.of_memo paths in
    let+ () = Action_builder.all_unit (List.map ~f:Action_builder.path paths) in
    paths
  | Other x -> x
;;

let dep_on_alias_rec alias ~loc =
  let src_dir = Path.Build.drop_build_context_exn (Alias.dir alias) in
  Action_builder.of_memo (Source_tree.find_dir src_dir)
  >>= function
  | None ->
    Action_builder.fail
      { fail =
          (fun () ->
            User_error.raise
              ~loc
              [ Pp.textf
                  "Don't know about directory %s!"
                  (Path.Source.to_string_maybe_quoted src_dir)
              ])
      }
  | Some _ ->
    let name = Dune_engine.Alias.name alias in
    Alias_rec.dep_on_alias_rec name (Alias.dir alias)
    >>| (function
     | Defined -> ()
     | Not_defined ->
       if not (Alias.is_standard name)
       then
         User_error.raise
           ~loc
           [ Pp.text "This alias is empty."
           ; Pp.textf
               "Alias %S is not defined in %s or any of its descendants."
               (Alias.Name.to_string name)
               (Path.Source.to_string_maybe_quoted src_dir)
           ])
;;

let expand_include ~dir ~project s =
  Path.Build.relative dir s
  |> Path.build
  |> Action_builder.read_sexp
  >>| function
  | List (_loc, asts) ->
    let dep_parser =
      Dune_lang.Syntax.set
        Stanza.syntax
        (Active (Dune_project.dune_version project))
        (String_with_vars.set_decoding_env
           (Pform.Env.initial Stanza.latest_version)
           (Bindings.decode Dep_conf.decode))
    in
    List.concat_map ~f:(Dune_lang.Decoder.parse dep_parser Univ_map.empty) asts
  | ast ->
    let loc = Dune_lang.Ast.loc ast in
    User_error.raise
      ~loc
      [ Pp.text "Dependency specification in `(include <filename>)` must be a list" ]
;;

let prepare_expander expander = Expander.set_expanding_what expander Deps_like_field

let add_sandbox_config acc (dep : Dep_conf.t) =
  match dep with
  | Sandbox_config cfg -> Sandbox_config.inter acc (make_sandboxing_config cfg)
  | _ -> acc
;;

let rec dir_contents ~loc d =
  let open Memo.O in
  Fs_memo.dir_contents d
  >>= function
  | Error e -> Unix_error.Detailed.raise e
  | Ok contents ->
    Fs_cache.Dir_contents.to_list contents
    |> Memo.parallel_map ~f:(fun (entry, kind) ->
      let path = Path.Outside_build_dir.relative d entry in
      match kind with
      | Unix.S_REG -> Memo.return [ path ]
      | S_DIR -> dir_contents ~loc path
      | _ ->
        User_error.raise
          ~loc
          [ Pp.text "Encountered a special file while expanding dependency." ])
    >>| List.concat
;;

let package loc pkg (context : Build_context.t) ~dune_version =
  let pkg = Package.Name.of_string pkg in
  Action_builder.of_memo
    (let open Memo.O in
     let* package_db = Package_db.create context.name in
     Package_db.find_package package_db pkg)
  >>= function
  | Some (Build build) -> build
  | Some (Local pkg) -> Alias_builder.alias (package_install ~context ~pkg)
  | Some (Installed pkg) ->
    if dune_version < (2, 9)
    then
      Action_builder.fail
        { fail =
            (fun () ->
              User_error.raise
                ~loc
                [ Pp.textf
                    "Dependency on an installed package requires at least (lang dune 2.9)"
                ])
        }
    else
      (let open Memo.O in
       Memo.parallel_map pkg.files ~f:(fun (s, l) ->
         let dir = Section.Map.find_exn pkg.sections s in
         Memo.parallel_map l ~f:(fun (kind, d) ->
           let path = Path.relative dir (Install.Entry.Dst.to_string d) in
           match kind with
           | `File -> Memo.return [ path ]
           | `Dir ->
             Path.as_outside_build_dir_exn path
             |> dir_contents ~loc
             >>| List.rev_map ~f:Path.outside_build_dir)
         >>| List.concat)
       >>| List.concat)
      |> Action_builder.of_memo
      >>= Action_builder.paths
  | None ->
    Action_builder.fail
      { fail =
          (fun () ->
            User_error.raise
              ~loc
              [ Pp.textf "Package %s does not exist" (Package.Name.to_string pkg) ])
      }
;;

let rec dep expander : Dep_conf.t -> _ = function
  | Include s ->
    (* TODO this is wrong. we shouldn't allow bindings here if we are in an
       unnamed expansion *)
    let dir = Expander.dir expander in
    Other
      (let* project = Action_builder.of_memo @@ Dune_load.find_project ~dir in
       let deps = expand_include ~dir ~project s in
       let* deps = deps in
       let builder, _bindings = named_paths_builder ~expander deps in
       let+ paths = builder in
       paths)
  | File s ->
    (match Expander.With_deps_if_necessary.expand_path expander s with
     | Without paths ->
       (* This special case is to support this pattern:

          {v
... (deps (:x foo)) (action (... (diff? %{x} %{x}.corrected))) ...
          v}

          Indeed, the second argument of [diff?] must be something that can be
          evaluated at rule production time since the dependency/target inferrer
          treats this argument as "consuming a target", and targets must be known
          at rule production time. This is not compatible with computing its
          expansion in the action builder monad, which is evaluated at rule
          execution time. *)
       Simple paths
     | With paths ->
       Other
         (let* paths = paths in
          let+ () = Action_builder.all_unit (List.map ~f:Action_builder.path paths) in
          paths))
  | Alias s ->
    Other
      (let* a = make_alias expander s in
       let+ () = Alias_builder.alias a in
       [])
  | Alias_rec s ->
    Other
      (let* a = make_alias expander s in
       let+ () = dep_on_alias_rec ~loc:(String_with_vars.loc s) a in
       [])
  | Glob_files glob_files ->
    Other
      (Glob_files_expand.action_builder
         glob_files
         ~f:(Expander.expand ~mode:Single expander)
         ~base_dir:(Expander.dir expander)
       >>| Glob_files_expand.Expanded.matches
       >>| List.map ~f:(fun path ->
         if Filename.is_relative path
         then Path.Build.relative (Expander.dir expander) path |> Path.build
         else Path.of_string path))
  | Source_tree s ->
    Other
      (let* path = Expander.expand_path expander s in
       let deps = Source_deps.files path in
       Action_builder.dyn_memo_deps deps |> Action_builder.map ~f:Path.Set.to_list)
  | Package p ->
    Other
      (let+ () =
         let* pkg = Expander.expand_str expander p in
         let context = Build_context.create ~name:(Expander.context expander) in
         let loc = String_with_vars.loc p in
         let* dune_version =
           Action_builder.of_memo
           @@
           let open Memo.O in
           Dune_load.find_project ~dir:(Expander.dir expander)
           >>| Dune_project.dune_version
         in
         package loc pkg context ~dune_version
       in
       [])
  | Universe ->
    Other
      (let+ () = Action_builder.dep Dep.universe in
       [])
  | Env_var var_sw ->
    Other
      (let* var = Expander.expand_str expander var_sw in
       let+ () = Action_builder.env_var var in
       [])
  | Sandbox_config _ -> Other (Action_builder.return [])

and named_paths_builder ~expander l =
  let builders, bindings =
    let expander = prepare_expander expander in
    List.fold_left l ~init:([], Pform.Map.empty) ~f:(fun (builders, bindings) x ->
      match x with
      | Bindings.Unnamed x -> to_action_builder (dep expander x) :: builders, bindings
      | Named (name, x) ->
        let x = List.map x ~f:(dep expander) in
        (match
           Option.List.all
             (List.map x ~f:(function
               | Simple x -> Some x
               | Other _ -> None))
         with
         | Some x ->
           let open Memo.O in
           let x = Memo.lazy_ (fun () -> Memo.all_concurrently x >>| List.concat) in
           let bindings =
             Pform.Map.set
               bindings
               (Var (User_var name))
               (Expander.Deps.Without (Memo.Lazy.force x >>| Value.L.paths))
           in
           let x =
             let open Action_builder.O in
             let* x = Action_builder.of_memo (Memo.Lazy.force x) in
             let+ () = Action_builder.paths x in
             x
           in
           x :: builders, bindings
         | None ->
           let x =
             Action_builder.memoize
               ~cutoff:(List.equal Path.equal)
               ("dep " ^ name)
               (Action_builder.List.concat_map x ~f:to_action_builder)
           in
           let bindings =
             Pform.Map.set
               bindings
               (Var (User_var name))
               (Expander.Deps.With (x >>| Value.L.paths))
           in
           x :: builders, bindings))
  in
  let builder = List.rev builders |> Action_builder.all >>| List.concat in
  builder, bindings
;;

let named ~expander l =
  let builder, bindings = named_paths_builder ~expander l in
  let builder =
    let builder =
      let+ paths = builder in
      Value.L.paths paths
    in
    Action_builder.memoize ~cutoff:(List.equal Value.equal) "deps" builder
  in
  let bindings = Pform.Map.set bindings (Var Deps) (Expander.Deps.With builder) in
  let expander = Expander.add_bindings_full expander ~bindings in
  ( Action_builder.ignore builder
  , expander
  , Bindings.fold l ~init:Sandbox_config.no_special_requirements ~f:(fun one acc ->
      match one with
      | Unnamed dep -> add_sandbox_config acc dep
      | Named (_, l) -> List.fold_left l ~init:acc ~f:add_sandbox_config) )
;;

let unnamed ?(sandbox = Sandbox_config.no_special_requirements) ~expander l =
  let expander = prepare_expander expander in
  ( List.fold_left l ~init:(Action_builder.return ()) ~f:(fun acc x ->
      let+ () = acc
      and+ _x = to_action_builder (dep expander x) in
      ())
  , List.fold_left l ~init:sandbox ~f:add_sandbox_config )
;;

let unnamed_get_paths ~expander l =
  let expander = prepare_expander expander in
  ( (let+ paths =
       List.fold_left l ~init:(Action_builder.return []) ~f:(fun acc x ->
         let+ acc = acc
         and+ paths = to_action_builder (dep expander x) in
         paths :: acc)
     in
     Path.Set.of_list (List.concat paths))
  , List.fold_left l ~init:None ~f:(fun acc (config : Dep_conf.t) ->
      match acc, config with
      | None, Sandbox_config _ ->
        Some
          (add_sandbox_config
             (Option.value ~default:Sandbox_config.no_special_requirements acc)
             config)
      | _, _ -> acc) )
;;
