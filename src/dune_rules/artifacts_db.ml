open Import
open Memo.O

let get_installed_binaries ~(context : Context.t) stanzas =
  let open Memo.O in
  let install_dir = Local_install_path.bin_dir ~context:context.name in
  let expand_str ~dir sw =
    Expander.With_reduced_var_set.expand_str ~context ~dir sw
  in
  let expand_str_partial ~dir sw =
    Expander.With_reduced_var_set.expand_str_partial ~context ~dir sw
  in
  Memo.List.map stanzas ~f:(fun (d : Dune_file.t) ->
      let dir = Path.Build.append_source context.build_dir d.dir in
      let binaries_from_install files =
        let* unexpanded_file_bindings =
          Dune_file.Install_conf.File_entry.to_file_bindings_unexpanded files
            ~expand_str:(expand_str ~dir) ~dir
        in
        Memo.List.map unexpanded_file_bindings ~f:(fun fb ->
            let+ p =
              File_binding.Unexpanded.destination_relative_to_install_path fb
                ~section:Bin ~expand:(expand_str ~dir)
                ~expand_partial:(expand_str_partial ~dir)
            in
            let p = Path.Local.of_string (Install.Dst.to_string p) in
            if Path.Local.is_root (Path.Local.parent_exn p) then
              Some (Path.Build.append_local install_dir p)
            else None)
        >>| List.filter_opt >>| Path.Build.Set.of_list
      in
      Memo.List.map d.stanzas ~f:(fun stanza ->
          match (stanza : Stanza.t) with
          | Dune_file.Install { section = Section Bin; files; _ } ->
            binaries_from_install files
          | Dune_file.Executables
              ({ install_conf = Some { section = Section Bin; files; _ }; _ } as
              exes) -> (
            let* enabled_if =
              Expander.With_reduced_var_set.eval_blang ~context ~dir
                exes.enabled_if
            in
            match enabled_if with
            | false -> Memo.return Path.Build.Set.empty
            | true -> (
              match exes.optional with
              | false -> binaries_from_install files
              | true ->
                let* compile_info =
                  let* scope = Scope.DB.find_by_dir dir in
                  let project = Scope.project scope in
                  let dune_version = Dune_project.dune_version project in
                  let+ pps =
                    Resolve.Memo.read_memo
                      (Preprocess.Per_module.with_instrumentation
                         exes.buildable.preprocess
                         ~instrumentation_backend:
                           (Lib.DB.instrumentation_backend (Scope.libs scope)))
                    >>| Preprocess.Per_module.pps
                  in
                  let merlin_ident =
                    Merlin_ident.for_exes ~names:(List.map ~f:snd exes.names)
                  in
                  Lib.DB.resolve_user_written_deps (Scope.libs scope)
                    (`Exe exes.names) exes.buildable.libraries ~pps
                    ~dune_version
                    ~allow_overlaps:
                      exes.buildable.allow_overlapping_dependencies
                    ~merlin_ident
                in
                let* available =
                  let open Memo.O in
                  let+ available = Lib.Compile.direct_requires compile_info in
                  Resolve.is_ok available
                in
                if available then binaries_from_install files
                else Memo.return Path.Build.Set.empty))
          | _ -> Memo.return Path.Build.Set.empty)
      >>| Path.Build.Set.union_all)
  >>| Path.Build.Set.union_all >>| Artifacts.Bin.Local.create

let all =
  Memo.lazy_ @@ fun () ->
  let+ contexts = Context.DB.all () in
  Context_name.Map.of_list_map_exn contexts ~f:(fun context ->
      let artifacts =
        Memo.lazy_ @@ fun () ->
        let* public_libs = Scope.DB.public_libs context in
        let* stanzas = Only_packages.filtered_stanzas context in
        let+ local_bins = get_installed_binaries ~context stanzas in
        Artifacts.create context ~public_libs ~local_bins
      in
      (context.name, artifacts))

let get (context : Context.t) =
  let* all = Memo.Lazy.force all in
  Context_name.Map.find_exn all context.name |> Memo.Lazy.force
