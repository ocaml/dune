open Import
open Memo.O

module Mounted_dune_files =
  Source_tree.Dir.Make_map_reduce
    (Memo)
    (Monoid.Product
       (Monoid.Appendable_list (struct
         type t = Dune_project.t * Source_tree.Dir.t
       end))
       (Monoid.Appendable_list (struct
            type t = Source_tree.Dir.t * Source.Dune_file.t
          end)))

let stanzas_by_package dune_files =
  Dune_file.fold_static_stanzas
    dune_files
    ~init:Package.Name.Map.empty
    ~f:(fun dune_file stanza acc ->
      let package =
        match Stanza.repr stanza with
        | Deprecated_library_name.T { old_name = old_public_name, _; _ } ->
          Some (Package.id (Public_lib.package old_public_name))
        | _ -> Stanzas.stanza_package stanza
      in
      match package with
      | None -> acc
      | Some package ->
        Package.Name.Map.add_multi acc (Package.Id.name package) (dune_file, stanza))
  |> Package.Name.Map.map ~f:List.rev
;;

(* The projects and dune files of one partition of a context: the workspace,
   or one mounted package. *)
module Partition = struct
  type t =
    { projects_by_output_root : Loaded_project.t Path.Build.Map.t
    ; dune_files : Dune_file.t list
    ; dune_file_by_dir : Dune_file.t Path.Build.Map.t
    ; stanzas_by_package : (Dune_file.t * Stanza.t) list Package.Name.Map.t Lazy.t
    }

  let make context_name projects dune_files =
    let projects_by_root =
      Path.Local.Map.of_list_map_exn projects ~f:(fun project ->
        Loaded_project.source_root project, project)
    in
    let* eval =
      Appendable_list.map dune_files ~f:(fun (dir, file) ->
        let root = Source_tree.Dir.project dir |> Dune_project.root in
        Path.Local.Map.find_exn projects_by_root root, dir, file)
      |> Dune_file.eval
    in
    let+ dune_files = eval context_name in
    { projects_by_output_root =
        Path.Build.Map.of_list_map_exn projects ~f:(fun project ->
          Loaded_project.output_root project, project)
    ; dune_files
    ; dune_file_by_dir =
        Path.Build.Map.of_list_map_exn dune_files ~f:(fun dune_file ->
          Dune_file.output_dir dune_file, dune_file)
    ; stanzas_by_package = lazy (stanzas_by_package dune_files)
    }
  ;;

  let projects_by_output_root t = t.projects_by_output_root
  let dune_files t = t.dune_files
end

let load_mount context candidate tree =
  let owner = Pkg_rules.Candidate.name candidate in
  let artifact_root = Pkg_rules.Candidate.artifact_root candidate in
  let version = (Pkg_rules.Candidate.lock_pkg candidate).info.version in
  let* projects, files =
    Mounted_dune_files.map_reduce
      tree
      ~traverse:Source_dir_status.Set.all
      ~trace_event_name:"Loaded mounted source tree"
      ~f:(fun dir ->
        let project = Source_tree.Dir.project dir in
        Memo.return
          ( (if
               Path.Local.equal
                 (Dune_project.root project)
                 (Source_tree.Dir.source_path dir)
             then Appendable_list.singleton (project, dir)
             else Appendable_list.empty)
          , match Source_tree.Dir.dune_file dir with
            | None -> Appendable_list.empty
            | Some file -> Appendable_list.singleton (dir, file) ))
  in
  let* projects =
    Appendable_list.to_list projects
    |> Memo.List.map ~f:(fun (project, source_tree_root) ->
      let project = Dune_project.set_package_version project ~package:owner ~version in
      let+ visible_packages =
        Dune_project.including_hidden_packages project
        |> Package.Name.Map.values
        |> Memo.List.filter ~f:(fun package ->
          if Package.Name.equal owner (Package.name package)
          then Dune_load.package_enabled package
          else Memo.return false)
        >>| Package.Name.Set.of_list_map ~f:Package.name
      in
      Loaded_project.create
        ~project
        ~owner:(Some owner)
        ~source_tree_root
        ~build_dir:(Context.build_dir context)
        ~partition_root:artifact_root
        ~stanza_mask:(Some visible_packages))
  in
  Partition.make (Context.name context) projects files
;;

(* One node per locked package: loading a mounted package fetches its source, so
   only what asks for the package may force it. *)
let mount =
  let memo =
    Memo.create
      "mounted-package"
      ~input:(module Pkg_rules.Context_and_package)
      (fun (context, name) ->
         let* locked = Pkg_rules.find_locked context name in
         match locked with
         | None | Some (_, `Opam) -> Memo.return None
         | Some (candidate, `Dune tree) ->
           let* context = Context.DB.get context in
           load_mount context candidate tree >>| Option.some)
  in
  fun context name -> Memo.exec memo (context, name)
;;

let workspace =
  Per_context.create_by_name ~name:"workspace-loaded-projects" (fun context_name ->
    Memo.lazy_ ~name:"workspace-loaded-projects-for-context" (fun () ->
      let* workspace = Dune_load.load ()
      and* context = Context.DB.get context_name in
      let stanza_mask =
        match Only_packages.enumerate workspace.mask with
        | `All -> None
        | `Set names -> Some names
      in
      let projects =
        List.map workspace.projects ~f:(fun (project, dir) ->
          Loaded_project.create
            ~project
            ~owner:None
            ~source_tree_root:dir
            ~build_dir:(Context.build_dir context)
            ~partition_root:(Context.build_dir context)
            ~stanza_mask)
      in
      Partition.make context_name projects workspace.dune_files)
    |> Memo.Lazy.force)
  |> Staged.unstage
;;

let find_package context name =
  let* { Dune_load.packages; _ } = Dune_load.load () in
  match Package.Name.Map.find packages name with
  | Some _ as package -> Memo.return package
  | None ->
    mount context name
    >>| (function
     | None -> None
     | Some { Partition.projects_by_output_root; _ } ->
       Path.Build.Map.values projects_by_output_root
       |> List.find_map ~f:(fun project ->
         Package.Name.Map.find (Loaded_project.packages project) name))
;;

let partition_of_dir context ~dir =
  let* mount =
    match Pkg_rules.package_of_artifact_path dir with
    | None -> Memo.return None
    | Some package -> mount context package
  in
  match mount with
  | Some partition -> Memo.return partition
  | None -> workspace context
;;

let find_loaded_project ~dir =
  let context = Install.Context.of_path dir |> Option.value_exn in
  let+ { Partition.projects_by_output_root; _ } = partition_of_dir context ~dir in
  Find_closest_source_dir.find_by_dir_exn projects_by_output_root ~dir
;;

let find_project ~dir =
  let+ project = find_loaded_project ~dir in
  Loaded_project.project project
;;

let stanzas_in_dir dir =
  if Path.Build.is_root dir
  then Memo.return None
  else (
    match Install.Context.of_path dir with
    | None -> Memo.return None
    | Some ctx ->
      let+ { Partition.dune_file_by_dir; _ } = partition_of_dir ctx ~dir in
      Path.Build.Map.find dune_file_by_dir dir)
;;

let package_closure_for_context context name =
  let+ source_packages = Dune_load.packages ()
  and+ managed = Pkg_rules.dependencies context in
  let rec loop acc name =
    if Package.Name.Set.mem acc name
    then acc
    else (
      let acc = Package.Name.Set.add acc name in
      let depends =
        match Package.Name.Map.find managed name with
        | Some depends -> depends
        | None ->
          (match Package.Name.Map.find source_packages name with
           | None -> []
           | Some package ->
             Package.depends package @ Package.depopts package
             |> List.map ~f:(fun (dep : Package_dependency.t) -> dep.name))
      in
      List.fold_left depends ~init:acc ~f:loop)
  in
  loop Package.Name.Set.empty name
;;

let dune_files context =
  let+ { Partition.dune_files; _ } = workspace context in
  dune_files
;;

(* The partition that may define a package's stanzas. *)
let partition_of_package context package =
  let* { Dune_load.packages; _ } = Dune_load.load () in
  if Package.Name.Map.mem packages package
  then workspace context >>| Option.some
  else mount context package
;;

let dune_files_of_package context package =
  partition_of_package context package
  >>| function
  | None -> []
  | Some { Partition.dune_files; _ } -> dune_files
;;

let stanzas_of_package context package =
  partition_of_package context package
  >>| function
  | None -> []
  | Some { Partition.stanzas_by_package; _ } ->
    Package.Name.Map.Multi.find (Lazy.force stanzas_by_package) package
;;

let source_tree_root ~dir =
  match Install.Context.of_path dir with
  | None -> Memo.return None
  | Some context ->
    let* valid = Per_context.valid context in
    if not valid
    then Memo.return None
    else (
      match Pkg_rules.package_of_artifact_path dir with
      | None -> Source_tree.root () >>| Option.some
      | Some package ->
        Pkg_rules.find_locked context package
        >>| (function
         | Some (_, `Dune tree) -> Some tree
         | None | Some (_, `Opam) -> None))
;;

let source_tree_dir ~dir =
  let* root = source_tree_root ~dir in
  match root with
  | None -> Memo.return None
  | Some root -> Source_tree.Dir.find_dir root (Dune_project.dir_in_context dir)
;;

let source_files ~dir =
  source_tree_dir ~dir
  >>| function
  | None -> Path.Build.Set.empty
  | Some source_dir ->
    Source_tree.Dir.filenames source_dir
    |> Filename.Array.Set.to_list
    |> Path.Build.Set.of_list_map ~f:(Path.Build.relative_fname dir)
;;

let vendored_warnings ~dir =
  let* project = find_loaded_project ~dir in
  if Loaded_project.vendored_warnings project
  then Memo.return true
  else
    Loaded_project.source_tree_dir project dir
    >>| function
    | None -> false
    | Some d -> Source_tree.Dir.status d = Vendored
;;
