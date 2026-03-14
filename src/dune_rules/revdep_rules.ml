open Import
open Memo.O

module Collect = struct
  type t =
    { libs : Lib.Set.t
    ; dirs : Path.Build.Set.t
    }

  let empty = { libs = Lib.Set.empty; dirs = Path.Build.Set.empty }
  let add_lib t lib = { t with libs = Lib.Set.add t.libs lib }
  let add_dir t dir = { t with dirs = Path.Build.Set.add t.dirs dir }

  let union a b =
    { libs = Lib.Set.union a.libs b.libs; dirs = Path.Build.Set.union a.dirs b.dirs }
  ;;
end

module Index = struct
  let add_dep acc dep dependent =
    Lib.Map.update acc dep ~f:(fun dep ->
      Some
        (match dep with
         | None -> dependent
         | Some prev -> Collect.union prev dependent))
  ;;

  let add_dep_for_libraries acc deps dir scope =
    Memo.List.fold_left deps ~init:acc ~f:(fun acc (dep : Lib_dep.t) ->
      match dep with
      (* Select deps are conditional and may resolve to different libs
         depending on availability, so we don't track them as revdeps. *)
      | Select _ -> Memo.return acc
      | Direct (loc, name) | Re_export (loc, name) | Instantiate { loc; lib = name; _ } ->
        Lib.DB.resolve_when_exists (Scope.libs scope) (loc, name)
        >>| (function
         | None -> acc
         | Some lib ->
           (match Resolve.peek lib with
            | Error () -> acc
            | Ok lib -> Collect.(add_dir empty dir) |> add_dep acc lib)))
  ;;

  let memo context_name =
    let open Memo.O in
    let* sctx = Super_context.find_exn context_name in
    let build_dir = Context_name.build_dir context_name in
    let process_dune_file acc dune_file =
      let src_dir = Dune_file.dir dune_file in
      let dir = Path.Build.append_source build_dir src_dir in
      let* scope = Scope.DB.find_by_dir dir in
      Dune_file.stanzas dune_file
      >>= Memo.List.fold_left ~init:acc ~f:(fun acc stanza ->
        match Stanza.repr stanza with
        | Library.T lib_conf ->
          (let lib_id = Library.to_lib_id ~src_dir lib_conf in
           Lib.DB.find_lib_id (Scope.libs scope) (Local lib_id))
          >>= (function
           | None -> Memo.return acc
           | Some lib ->
             let+ deps_ocaml = Lib.requires lib ~for_:Ocaml
             and+ deps_melange = Lib.requires lib ~for_:Melange in
             let deps =
               let deps deps =
                 match Resolve.peek deps with
                 | Ok deps -> deps
                 | Error () -> []
               in
               List.rev_append (deps deps_ocaml) (deps deps_melange)
               |> List.filter ~f:Lib.is_local
             in
             List.fold_left deps ~init:acc ~f:(fun acc dep ->
               add_dep acc dep Collect.(add_lib empty lib)))
        | Executables.T { enabled_if; buildable = { libraries; _ }; _ }
        | Tests.T { exes = { enabled_if; buildable = { libraries; _ }; _ }; _ } ->
          let* expander = Super_context.expander sctx ~dir in
          Expander.eval_blang expander enabled_if
          >>= (function
           | false -> Memo.return acc
           | true -> add_dep_for_libraries acc libraries dir scope)
        | _ -> Memo.return acc)
    in
    Dune_load.dune_files context_name
    >>= Memo.List.fold_left ~init:Lib.Map.empty ~f:process_dune_file
  ;;

  let memo = Memo.create "revdep-index" ~input:(module Context_name) memo

  let dependents context_name libs =
    let+ index = Memo.exec memo context_name in
    List.fold_left libs ~init:Collect.empty ~f:(fun acc lib ->
      match Lib.Map.find index lib with
      | None -> acc
      | Some deps -> Collect.union acc deps)
  ;;
end

let libs_in_dir ~scope ~dir =
  Dune_load.stanzas_in_dir dir
  >>= function
  | None -> Memo.return []
  | Some dune_file ->
    Dune_file.stanzas dune_file
    >>= Memo.List.filter_map ~f:(fun stanza ->
      match Stanza.repr stanza with
      | Library.T lib ->
        let lib_id =
          let src_dir = Dune_file.dir dune_file in
          Library.to_lib_id ~src_dir lib
        in
        Lib.DB.find_lib_id (Scope.libs scope) (Local lib_id)
      | _ -> Memo.return None)
;;

let add ~sctx ~dir =
  let* libs_here =
    let* scope = Scope.DB.find_by_dir dir in
    libs_in_dir ~scope ~dir
  in
  let context_name = Context.name (Super_context.context sctx) in
  let build_revdep_alias alias_name target_alias =
    let alias = Alias.make alias_name ~dir in
    (match libs_here with
     | [] -> Action_builder.return ()
     | libs ->
       let open Action_builder.O in
       let* (deps : Collect.t) =
         Action_builder.of_memo (Index.dependents context_name libs)
       in
       let lib_targets =
         (* Safe to use of_lib_exn: the index only contains local libs
             (filtered by Lib.is_local in Index.memo). *)
         Lib.Set.to_list_map deps.libs ~f:(fun lib ->
           let dir = lib |> Lib.Local.of_lib_exn |> Lib.Local.info |> Lib_info.src_dir in
           Alias.make target_alias ~dir |> Dep.alias |> Action_builder.dep)
       in
       let dir_targets =
         Path.Build.Set.to_list_map deps.dirs ~f:(fun dir ->
           Alias.make target_alias ~dir |> Dep.alias |> Action_builder.dep)
       in
       Action_builder.all_unit (lib_targets @ dir_targets))
    |> Rules.Produce.Alias.add_deps alias
  in
  let* () = build_revdep_alias Alias0.revdep Alias0.default in
  let* () = build_revdep_alias Alias0.revdep_check Alias0.check in
  let* () = build_revdep_alias Alias0.revdep_runtest Alias0.runtest in
  build_revdep_alias Alias0.revdep_install Alias0.install
;;
