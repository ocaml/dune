open Import
open Memo.O

let available_exes ~dir (exes : Executables.t) =
  let* compile_info =
    let* scope = Scope.DB.find_by_dir dir in
    let* scope = Scope.DB.for_package scope exes.package in
    (* If preprocessors fail to resolve, this fails when loading artifacts
       instead of making the binary unavailable. *)
    Scope.resolve_executable_deps scope ~db:(Scope.libs scope) exes
  in
  (* CR-someday rgrinberg: what if a preprocessor is unavailable? *)
  let+ available = Lib.Compile.direct_requires compile_info ~for_:Ocaml in
  Resolve.is_ok available
;;

let get_installed_binaries stanzas =
  let merge _ x y = Some (Appendable_list.( @ ) x y) in
  Memo.List.map stanzas ~f:(fun d ->
    let dir = Dune_file.output_dir d in
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
    let binaries_from_install ~enabled_if ~package files =
      let* unexpanded_file_bindings =
        let* source_tree_root = Partitions.source_tree_root ~dir >>| Option.value_exn in
        Install_entry.File.to_file_bindings_unexpanded
          files
          ~expand:expand_value
          ~dir
          ~source_tree_root
      in
      Memo.List.map unexpanded_file_bindings ~f:(fun fb ->
        let+ p =
          File_binding_expand.destination_relative_to_install_path
            fb
            ~section:Bin
            ~expand:expand_str
            ~expand_partial:expand_str_partial
        in
        let dst = Install.Entry.Dst.local p in
        if Path.Local.is_root (Path.Local.parent_exn dst)
        then (
          let origin = { Artifacts.binding = fb; dir; dst; enabled_if; package } in
          Some (Path.Local.basename dst, origin))
        else None)
      >>| List.filter_opt
      >>| Filename.Map.of_list_reduce ~f:(fun _ y ->
        (* CR-someday rgrinberg: we shouldn't allow duplicate bindings, but where's the
           correct place for this validation? *)
        y)
      >>| Filename.Map.map ~f:Appendable_list.singleton
    in
    Dune_file.static_stanzas d
    |> Memo.List.map ~f:(fun stanza ->
      match Stanza.repr stanza with
      | Install_conf.T { section = _loc, Section Bin; files; enabled_if; package; _ } ->
        let enabled_if = eval_blang enabled_if in
        let package = Some (Package.name package) in
        binaries_from_install ~enabled_if ~package files
      | Executables.T
          ({ install_conf = Some { section = _loc, Section Bin; files; package; _ }; _ }
           as exes) ->
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
        let package = Some (Package.name package) in
        binaries_from_install ~enabled_if ~package files
      | _ -> Memo.return Filename.Map.empty)
    >>| Filename.Map.union_all ~f:merge)
  >>| Filename.Map.union_all ~f:merge
;;

(* The binaries a natively built locked package installs, under the names a
   lookup uses. *)
let package_binaries =
  let memo =
    Memo.create
      "package-binaries"
      ~input:(module Pkg_rules.Context_and_package)
      (fun (context, package) ->
         let+ binaries =
           Partitions.dune_files_of_package context package >>= get_installed_binaries
         in
         Filename.Map.to_list_map binaries ~f:(fun name sources ->
           Filename.of_string_exn (Bin.strip_exe (Filename.to_string name)), sources)
         |> Filename.Map.of_list_reduce ~f:Appendable_list.( @ ))
  in
  fun context package -> Memo.exec memo (context, package)
;;

(* A lookup only loads the packages it may see and the compiler's, never
   building an opaque one to see what it installs. *)
let native_bins context ~packages name =
  let* active = Lock_dir.lock_dir_active context in
  if not active
  then Memo.return []
  else
    let* declared = Pkg_rules.declared_packages context packages in
    Package.Name.Set.to_list declared
    |> Memo.List.concat_map ~f:(fun package ->
      let* locked = Pkg_rules.find_locked context package in
      match locked with
      | None | Some (_, `Opam) -> Memo.return []
      | Some (_, `Dune _) ->
        let+ binaries = package_binaries context package in
        Filename.Map.find binaries name
        |> Option.map ~f:Appendable_list.to_list
        |> Option.value ~default:[])
;;

let all =
  Memo.lazy_ ~name:"Artifacts_db.all"
  @@ fun () ->
  let+ contexts = Context.DB.all () in
  Context_name.Map.of_list_map_exn contexts ~f:(fun context ->
    let artifacts =
      let local_bins =
        Memo.lazy_ ~name:"get_installed_binaries" (fun () ->
          Context.name context |> Partitions.dune_files >>= get_installed_binaries)
      in
      Artifacts.create
        context
        ~local_bins
        ~native_bins:(native_bins (Context.name context))
      |> Memo.return
    in
    Context.name context, artifacts)
;;

let get (context : Context.t) =
  let* all = Memo.Lazy.force all in
  Context_name.Map.find_exn all (Context.name context)
;;
