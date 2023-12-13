open Import
open Memo.O

let available_exes ~dir (exes : Dune_file.Executables.t) =
  let* compile_info =
    let* scope = Scope.DB.find_by_dir dir in
    let dune_version =
      let project = Scope.project scope in
      Dune_project.dune_version project
    in
    let+ pps =
      (* Instead of making the binary unavailable, this will just
         fail when loading artifacts. This is clearly bad but
         "optional" executables shouldn't be used. *)
      Resolve.Memo.read_memo
        (Preprocess.Per_module.with_instrumentation
           exes.buildable.preprocess
           ~instrumentation_backend:(Lib.DB.instrumentation_backend (Scope.libs scope)))
      >>| Preprocess.Per_module.pps
    in
    let merlin_ident = Merlin_ident.for_exes ~names:(List.map ~f:snd exes.names) in
    Lib.DB.resolve_user_written_deps
      (Scope.libs scope)
      (`Exe exes.names)
      exes.buildable.libraries
      ~pps
      ~dune_version
      ~forbidden_libraries:exes.forbidden_libraries
      ~allow_overlaps:exes.buildable.allow_overlapping_dependencies
      ~merlin_ident
  in
  let open Memo.O in
  let+ available = Lib.Compile.direct_requires compile_info in
  Resolve.is_ok available
;;

let get_installed_binaries ~(context : Context.t) stanzas =
  let open Memo.O in
  let install_dir = Install.Context.bin_dir ~context:(Context.name context) in
  let expand_str ~dir sw = Expander.With_reduced_var_set.expand_str ~dir sw in
  let expand_str_partial ~dir sw =
    Expander.With_reduced_var_set.expand_str_partial ~dir sw
  in
  Memo.List.map stanzas ~f:(fun (d : Dune_file.t) ->
    let dir = Path.Build.append_source (Context.build_dir context) d.dir in
    let binaries_from_install files =
      let* unexpanded_file_bindings =
        Install_entry.File.to_file_bindings_unexpanded
          files
          ~expand_str:(expand_str ~dir)
          ~dir
      in
      Memo.List.map unexpanded_file_bindings ~f:(fun fb ->
        let+ p =
          File_binding.Unexpanded.destination_relative_to_install_path
            fb
            ~section:Bin
            ~expand:(expand_str ~dir)
            ~expand_partial:(expand_str_partial ~dir)
        in
        let p = Path.Local.of_string (Install.Entry.Dst.to_string p) in
        if Path.Local.is_root (Path.Local.parent_exn p)
        then Some (Path.Build.append_local install_dir p)
        else None)
      >>| List.filter_opt
      >>| Path.Build.Set.of_list
    in
    Memo.List.map d.stanzas ~f:(fun stanza ->
      match Stanza.repr stanza with
      | Install_conf.T { section = Section Bin; files; _ } -> binaries_from_install files
      | Dune_file.Executables.T
          ({ install_conf = Some { section = Section Bin; files; _ }; _ } as exes) ->
        let* available =
          let* enabled_if =
            Expander.With_reduced_var_set.eval_blang ~dir exes.enabled_if
          in
          match enabled_if with
          | false -> Memo.return false
          | true ->
            (match exes.optional with
             | false -> Memo.return true
             | true -> available_exes ~dir exes)
        in
        if available
        then binaries_from_install files
        else Memo.return Path.Build.Set.empty
      | _ -> Memo.return Path.Build.Set.empty)
    >>| Path.Build.Set.union_all)
  >>| Path.Build.Set.union_all
;;

let all =
  Memo.lazy_ ~name:"Artifacts_db.all"
  @@ fun () ->
  let+ contexts = Context.DB.all () in
  Context_name.Map.of_list_map_exn contexts ~f:(fun context ->
    let artifacts =
      let local_bins =
        Memo.lazy_ ~name:"get_installed_binaries" (fun () ->
          let* stanzas = Only_packages.filtered_stanzas (Context.name context) in
          get_installed_binaries ~context stanzas)
      in
      Artifacts.create context ~local_bins |> Memo.return
    in
    Context.name context, artifacts)
;;

let get (context : Context.t) =
  let* all = Memo.Lazy.force all in
  Context_name.Map.find_exn all (Context.name context)
;;
