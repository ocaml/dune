open Import
open Memo.O

let available_exes ~dir (exes : Executables.t) =
  let* compile_info =
    let* scope = Scope.DB.find_by_dir dir in
    let dune_version =
      let project = Scope.project scope in
      Dune_project.dune_version project
    in
    let libs = Scope.libs scope in
    let+ pps =
      (* Instead of making the binary unavailable, this will just
         fail when loading artifacts. This is clearly bad but
         "optional" executables shouldn't be used. *)
      Preprocess.Per_module.with_instrumentation
        exes.buildable.preprocess
        ~instrumentation_backend:(Lib.DB.instrumentation_backend libs)
      |> Resolve.Memo.read_memo
      >>| Preprocess.Per_module.pps
    in
    let names = Nonempty_list.to_list exes.names in
    let merlin_ident = Merlin_ident.for_exes ~names:(List.map ~f:snd names) in
    Lib.DB.resolve_user_written_deps
      libs
      (`Exe names)
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
  let merge _ x y = Some (Appendable_list.( @ ) x y) in
  let open Memo.O in
  Memo.List.map stanzas ~f:(fun d ->
    let dir = Path.Build.append_source (Context.build_dir context) (Dune_file.dir d) in
    let* expander = Expander0.get ~dir in
    let expand_value sw =
      Expander0.expand expander ~mode:Single sw
      |> Action_builder.evaluate_and_collect_facts
      >>| fst
    in
    let expand_str sw =
      Expander0.expand_str expander sw
      |> Action_builder.evaluate_and_collect_facts
      >>| fst
    in
    let expand_str_partial sw =
      Expander0.expand_str_partial expander sw
      |> Action_builder.evaluate_and_collect_facts
      >>| fst
    in
    let eval_blang = Expander0.eval_blang expander in
    let binaries_from_install ~enabled_if files =
      let* unexpanded_file_bindings =
        Install_entry.File.to_file_bindings_unexpanded files ~expand:expand_value ~dir
      in
      Memo.List.map unexpanded_file_bindings ~f:(fun fb ->
        let+ p =
          File_binding.Unexpanded.destination_relative_to_install_path
            fb
            ~section:Bin
            ~expand:expand_str
            ~expand_partial:expand_str_partial
        in
        let dst = Path.Local.of_string (Install.Entry.Dst.to_string p) in
        if Path.Local.is_root (Path.Local.parent_exn dst)
        then (
          let origin = { Artifacts.binding = fb; dir; dst; enabled_if } in
          Some (Path.Local.basename dst, origin))
        else None)
      >>| List.filter_opt
      >>| Filename.Map.of_list_reduce ~f:(fun _ y ->
        (* CR-rgrinberg: we shouldn't allow duplicate bindings, but where's the
           correct place for this validation? *)
        y)
      >>| Filename.Map.map ~f:Appendable_list.singleton
    in
    Dune_file.static_stanzas d
    |> Memo.List.map ~f:(fun stanza ->
      match Stanza.repr stanza with
      | Install_conf.T { section = _loc, Section Bin; files; enabled_if; _ } ->
        let enabled_if = eval_blang enabled_if in
        binaries_from_install ~enabled_if files
      | Executables.T
          ({ install_conf = Some { section = _loc, Section Bin; files; _ }; _ } as exes)
        ->
        let enabled_if =
          let enabled_if = eval_blang exes.enabled_if in
          match exes.optional with
          | false -> enabled_if
          | true ->
            enabled_if
            >>= (function
             | false -> Memo.return false
             | true -> available_exes ~dir exes)
        in
        binaries_from_install ~enabled_if files
      | _ -> Memo.return Filename.Map.empty)
    >>| Filename.Map.union_all ~f:merge)
  >>| Filename.Map.union_all ~f:merge
;;

let all =
  Memo.lazy_ ~name:"Artifacts_db.all"
  @@ fun () ->
  let+ contexts = Context.DB.all () in
  Context_name.Map.of_list_map_exn contexts ~f:(fun context ->
    let artifacts =
      let local_bins =
        Memo.lazy_ ~name:"get_installed_binaries" (fun () ->
          Context.name context |> Dune_load.dune_files >>= get_installed_binaries ~context)
      in
      Artifacts.create context ~local_bins |> Memo.return
    in
    Context.name context, artifacts)
;;

let get (context : Context.t) =
  let* all = Memo.Lazy.force all in
  Context_name.Map.find_exn all (Context.name context)
;;
