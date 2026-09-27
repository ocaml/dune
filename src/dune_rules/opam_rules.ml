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
  let source_files =
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
    source @ extra
  in
  let build =
    let open Action_builder.O in
    let* layout = Package_layout.materialize context closure in
    let* base = Action_builder.of_memo (Context.base_env context_value) in
    let* env = Install_layout.env layout ~path:(Context.path context_value) ~base in
    let dependencies = Install_layout.dependencies layout in
    let env =
      Env.add
        env
        ~var:(Env.Var.of_string "OPAM_PACKAGE_NAME")
        ~value:(Package.Name.to_string paths.name)
      |> Env.add
           ~var:(Env.Var.of_string "OPAM_PACKAGE_VERSION")
           ~value:(Package_version.to_string stanza.version)
      |> Env.add ~var:(Env.Var.of_string "OPAMCLI") ~value:"2.0"
      |> Env.add ~var:(Env.Var.of_string "OPAMSWITCH") ~value:"dune"
      |> Env.add ~var:(Env.Var.of_string "CDPATH") ~value:""
      |> Env.add ~var:(Env.Var.of_string "MAKELEVEL") ~value:""
      |> Env.add
           ~var:Opam_switch.opam_switch_prefix_var_name
           ~value:(Path.to_absolute_filename paths.prefix)
    in
    let direct_dependencies =
      List.filter dependencies ~f:(fun (dep : Opam_build.Dependency.t) ->
        Package.Name.Set.mem direct dep.paths.name)
    in
    let depends =
      List.map direct_dependencies ~f:(fun (dep : Opam_build.Dependency.t) ->
        dep.paths.name, (dep.variables, dep.paths))
      |> Package.Name.Map.of_list_exn
      |> fun deps ->
      Package.Name.Map.add_exn
        deps
        paths.name
        ( Opam_build.variables ~name:paths.name ~version:stanza.version ~dev:stanza.dev
        , paths )
    in
    let artifacts =
      List.concat_map dependencies ~f:Opam_build.Dependency.binaries
      |> Filename.Map.of_list_map_exn ~f:(fun path ->
        ( Filename.of_string_exn (Bin.strip_exe (Filename.to_string (Path.basename path)))
        , path ))
    in
    let files =
      List.map direct_dependencies ~f:(fun (dep : Opam_build.Dependency.t) ->
        dep.paths.name, dep.files)
      |> Package.Name.Map.of_list_exn
    in
    let expander =
      { Opam_build.Action_expander.Expander.name = paths.name
      ; paths
      ; depends = Memo.return depends
      ; files = Memo.return files
      ; artifacts = Memo.return artifacts
      ; depexts = stanza.recipe.depexts
      ; context
      ; path = Context.path context_value
      ; version = stanza.version
      ; env = Env.to_map env |> Env.Map.map ~f:(fun value -> [ Value.String value ])
      }
    in
    let* build, install =
      Action_builder.of_memo
        (let open Memo.O in
         let expand = function
           | None -> Memo.return []
           | Some action ->
             let+ action = Opam_build.Action_expander.expand action ~expander in
             [ Action.chdir paths.source_dir action
               |> Action.Full.make
               |> Action_builder.With_targets.return
             ]
         in
         Memo.both (expand stanza.recipe.build) (expand stanza.recipe.install))
    in
    let { Action_builder.With_targets.build; targets = _ } =
      Opam_build.execute ~expander ~write_paths ~source_files ~build ~install
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
