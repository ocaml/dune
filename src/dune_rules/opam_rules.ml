open Import
open Memo.O

let rule (package : Opam_package.t) =
  let { Opam_package.context; stanza; paths = write_paths; dir } = package in
  let* context_value = Context.DB.get context in
  (match Context.for_host context_value with
   | None -> ()
   | Some _ ->
     User_error.raise
       ~loc:stanza.loc
       [ Pp.text "Opam builds are not supported in cross-compilation target contexts." ]);
  let* sctx = Super_context.find_exn context in
  let* source_expander = Super_context.expander sctx ~dir in
  let direct =
    Package.depends stanza.package
    |> Package.Name.Set.of_list_map ~f:(fun (dep : Package_dependency.t) -> dep.name)
  in
  let* closure = Dune_load.package_closure direct in
  let* packages = Dune_load.packages () in
  let check_required package =
    List.iter (Package.depends package) ~f:(fun (dep : Package_dependency.t) ->
      if not (Package.Name.Map.mem packages dep.name)
      then
        User_error.raise
          ~loc:stanza.loc
          [ Pp.textf
              "Dependency %S of package %S is not a workspace package."
              (Package.Name.to_string dep.name)
              (Package.Name.to_string (Package.name package))
          ])
  in
  check_required stanza.package;
  let closure =
    Package.Name.Set.filter closure ~f:(fun name ->
      match Package.Name.Map.find packages name with
      | None -> false
      | Some package ->
        check_required package;
        true)
  in
  let paths = Opam_build.Paths.map_path write_paths ~f:Path.build in
  let source =
    let open Action_builder.O in
    let* source =
      match stanza.source with
      | None -> Action_builder.return []
      | Some source ->
        let* root = Expander.expand_path source_expander source in
        let+ files = Action_builder.dyn_memo_deps (Source_deps.files root) in
        Path.Set.to_list_map files ~f:(fun file ->
          file, Path.drop_prefix_exn file ~prefix:root)
    in
    let+ extra =
      Action_builder.List.map stanza.files ~f:(fun (src, dst) ->
        let+ src = Expander.expand_path source_expander src in
        src, dst)
    in
    { Opam_build.Source_input.files = source @ extra; directories = [] }
  in
  let build =
    let open Action_builder.O in
    let* layout = Package_layout.materialize context closure in
    let* base = Action_builder.of_memo (Context.base_env context_value) in
    let* env = Install_layout.env layout ~path:(Context.path context_value) ~base in
    let expander =
      Opam_build.Action_expander.for_recipe
        ~context
        ~path:(Context.path context_value)
        ~paths
        ~version:stanza.version
        ~variables:
          (Opam_build.variables ~name:paths.name ~version:stanza.version ~dev:stanza.dev)
        ~depexts:stanza.recipe.depexts
        ~env
        ~direct
        ~dependencies:(Install_layout.dependencies layout)
    in
    let* { Action_builder.With_targets.build; targets = _ } =
      Action_builder.of_memo
        (Opam_build.execute_recipe stanza.recipe ~expander ~write_paths ~source)
    in
    build
  in
  let { Action_builder.With_targets.build; targets } =
    Action_builder.with_no_targets build
    |> Action_builder.With_targets.add_directories
         ~directory_targets:[ write_paths.target_dir ]
  in
  Rules.Produce.rule (Rule.make ~info:(From_dune_file stanza.loc) ~targets build)
;;

let gen_rules context ~dir rest =
  let module G = Build_config.Gen_rules in
  match rest with
  | [] ->
    Memo.return
      (G.make
         ~build_dir_only_sub_dirs:(G.Build_only_sub_dirs.singleton ~dir Subdir_set.all)
         (Memo.return Rules.empty))
  | [ key ] ->
    let* packages = Opam_package.all context in
    (match
       Package.Name.Map.values packages
       |> List.find ~f:(fun package -> String.equal (Opam_package.key package) key)
     with
     | None -> Memo.return G.no_rules
     | Some package ->
       Memo.return
         (G.make
            ~build_dir_only_sub_dirs:
              (G.Build_only_sub_dirs.singleton ~dir Subdir_set.empty)
            ~directory_targets:
              (Path.Build.Map.singleton package.paths.target_dir package.stanza.loc)
            (Rules.collect_unit (fun () -> rule package))))
  | _ -> Memo.return (G.Redirect_to_parent G.Rules.empty)
;;
