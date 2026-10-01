open Import
open Memo.O

module Dune_file_db = struct
  type t = Dune_file.t Path.Build.Map.t

  let make all =
    Path.Build.Map.of_list_map_exn all ~f:(fun dune_file ->
      Dune_file.output_dir dune_file, dune_file)
  ;;
end

let load = Dune_load_workspace.load

type loaded =
  { dune_files : Dune_file.t list
  ; projects_by_output_root : Loaded_project.t Path.Build.Map.t
  ; dune_file_by_dir : Dune_file_db.t
  ; mounted_errors : User_message.t list Package.Name.Map.t
  }

module Mounted_dune_files =
  Source_tree.Rules.Dir.Make_map_reduce
    (Memo)
    (Monoid.Appendable_list (struct
         type t = Source_tree.Rules.Dir.t * Source.Dune_file.t
       end))

let load_mounted context mask =
  let* mounted = Pkg_sources.mounted (Context.name context) in
  Memo.List.map mounted ~f:(fun mounted ->
    let owner = Pkg_sources.Mounted.candidate mounted |> Pkg_sources.Candidate.name in
    let+ result =
      Memo.collect_user_errors
        (Memo.of_thunk (fun () ->
           let* _kind = Pkg_sources.Mounted.kind_exn mounted in
           let* projects, files =
             match Pkg_sources.Mounted.tree mounted with
             | None -> Memo.return ([], Appendable_list.empty)
             | Some tree ->
               let candidate = Pkg_sources.Mounted.candidate mounted in
               let owner = Pkg_sources.Candidate.name candidate in
               let artifact_root = Pkg_sources.Candidate.artifact_root candidate in
               let partition =
                 Build_partition.mounted ~resolver:context ~output_root:artifact_root
               in
               let* projects =
                 Pkg_sources.Mounted.projects mounted
                 |> Memo.List.map ~f:(fun (project, source_tree_root) ->
                   let* visible_packages =
                     match
                       Package.Name.Map.find
                         (Dune_project.including_hidden_packages project)
                         owner
                     with
                     | None -> Memo.return Package.Name.Set.empty
                     | Some package ->
                       let+ enabled = Package_enabled.eval package in
                       if enabled
                       then Package.Name.Set.singleton owner
                       else Package.Name.Set.empty
                   in
                   let source_root = Dune_project.root project in
                   let project_root =
                     Source_path.descendant
                       source_root
                       ~of_:(Source_path.build artifact_root)
                     |> Option.value_exn
                   in
                   Loaded_project.create
                     ~project
                     ~identity:
                       (Loaded_project.Identity.mounted ~package:owner ~project_root)
                     ~source_root
                     ~source_tree_root
                     ~partition
                     ~output_root:(Path.Build.append_local artifact_root project_root)
                     ~visible_packages:(Some visible_packages)
                   |> Memo.return)
               in
               let projects_by_root =
                 Source_path.Map.of_list_map_exn projects ~f:(fun project ->
                   Loaded_project.source_root project, project)
               in
               let+ files =
                 Mounted_dune_files.map_reduce
                   (Source_tree.Rules.Mounted.root tree)
                   ~traverse:Source_dir_status.Set.all
                   ~trace_event_name:"Loaded mounted source tree"
                   ~f:(fun dir ->
                     match Source_tree.Rules.Dir.dune_file dir with
                     | None -> Memo.return Appendable_list.empty
                     | Some file -> Memo.return (Appendable_list.singleton (dir, file)))
               in
               let files =
                 Appendable_list.map files ~f:(fun (dir, file) ->
                   let root = Source_tree.Rules.Dir.project dir |> Dune_project.root in
                   Source_path.Map.find_exn projects_by_root root, dir, file)
               in
               projects, files
           in
           let* eval = Dune_file.eval files mask in
           let+ files = eval (Context.name context) in
           projects, files))
    in
    owner, result)
  >>| List.fold_left
        ~init:([], [], Package.Name.Map.empty)
        ~f:(fun (projects, files, errors) (name, result) ->
          match result with
          | Ok (loaded_projects, loaded_files) ->
            loaded_projects @ projects, loaded_files @ files, errors
          | Error messages ->
            projects, files, Package.Name.Map.add_exn errors name messages)
;;

let loaded =
  let by_context =
    Per_context.create_by_name ~name:"workspace-loaded-projects" (fun context_name ->
      Memo.lazy_ ~name:"workspace-loaded-projects-for-context" (fun () ->
        let* workspace = load ()
        and* context = Context.DB.get context_name in
        let partition = Build_partition.workspace context in
        let projects =
          List.map workspace.projects ~f:(fun project ->
            let source_root = Dune_project.root project in
            let root = Source_path.as_workspace source_root |> Option.value_exn in
            Loaded_project.create
              ~project
              ~identity:(Loaded_project.Identity.workspace root)
              ~source_root
              ~source_tree_root:
                (Source_path.Map.find_exn workspace.project_dirs source_root)
              ~partition
              ~output_root:(Path.Build.append_source (Context.build_dir context) root)
              ~visible_packages:None)
        in
        let projects_by_root =
          Source_path.Map.of_list_map_exn projects ~f:(fun project ->
            Loaded_project.source_root project, project)
        in
        let source_dune_files =
          Appendable_list.map workspace.dune_files ~f:(fun (dir, project, dune_file) ->
            let project =
              Source_path.Map.find_exn projects_by_root (Dune_project.root project)
            in
            project, dir, dune_file)
        in
        let* mounted_projects, mounted_files, mounted_errors =
          load_mounted context workspace.mask
        in
        let projects = projects @ mounted_projects in
        let* eval = Dune_file.eval source_dune_files workspace.mask in
        let+ dune_files = eval context_name in
        let dune_files = dune_files @ mounted_files in
        let projects_by_output_root =
          Path.Build.Map.of_list_map_exn projects ~f:(fun project ->
            Loaded_project.output_root project, project)
        in
        { dune_files
        ; projects_by_output_root
        ; dune_file_by_dir = Dune_file_db.make dune_files
        ; mounted_errors
        })
      |> Memo.Lazy.force)
    |> Staged.unstage
  in
  fun context -> by_context context
;;

let package_errors context =
  let+ { mounted_errors; _ } = loaded context in
  mounted_errors
;;

let check_package context name =
  let* { mounted_errors; _ } = loaded context in
  match Package.Name.Map.find mounted_errors name with
  | None -> Memo.return ()
  | Some messages ->
    Memo.parallel_iter messages ~f:(fun message -> raise (User_error.E message))
;;

let projects_by_output_root context =
  let+ { projects_by_output_root; _ } = loaded context in
  projects_by_output_root
;;

let packages_for_context context =
  let+ projects = projects_by_output_root context in
  Path.Build.Map.values projects
  |> List.concat_map ~f:(fun project ->
    Dune_project.packages (Loaded_project.project project)
    |> Package.Name.Map.values
    |> List.filter ~f:(fun package ->
      match Loaded_project.owner project with
      | None -> true
      | Some owner -> Package.Name.equal owner (Package.name package)))
  |> Package.Name.Map.of_list_map_exn ~f:(fun package -> Package.name package, package)
;;

let context_of_dir dir =
  match Path.Build.extract_build_context dir with
  | Some (context, source_dir) ->
    let context = Context_name.of_string (Filename.to_string context) in
    if Context_name.equal context Private_context.t.name
    then (
      match Path.Source.explode source_dir with
      | context :: _ -> Context_name.of_string (Filename.to_string context)
      | [] ->
        Code_error.raise
          "Dune_load: private path has no resolver context"
          [ "dir", Path.Build.to_dyn dir ])
    else context
  | None ->
    Code_error.raise
      "Dune_load: path is not in a build context"
      [ "dir", Path.Build.to_dyn dir ]
;;

let find_loaded_project ~dir =
  let context = context_of_dir dir in
  let+ projects_by_output_root = projects_by_output_root context in
  let rec loop dir =
    match Path.Build.Map.find projects_by_output_root dir with
    | Some project -> project
    | None ->
      (match Path.Build.parent dir with
       | Some parent -> loop parent
       | None ->
         Code_error.raise
           "Dune_load: no enclosing loaded project"
           [ "dir", Path.Build.to_dyn dir ])
  in
  loop dir
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
      let+ { dune_file_by_dir; _ } = loaded ctx in
      Path.Build.Map.find dune_file_by_dir dir)
;;

let mask () =
  let+ { mask; _ } = load () in
  mask
;;

let mask_for_dir ~dir =
  let* project = find_loaded_project ~dir in
  match Loaded_project.visible_packages project with
  | Some packages -> Memo.return (`Set packages)
  | None -> mask () >>| Only_packages.enumerate
;;

let packages () =
  let+ { packages; _ } = load () in
  packages
;;

let package_closure names =
  let+ packages = packages () in
  let rec loop acc name =
    if Package.Name.Set.mem acc name
    then acc
    else (
      let acc = Package.Name.Set.add acc name in
      match Package.Name.Map.find packages name with
      | None -> acc
      | Some pkg ->
        List.fold_left
          (Package.depends pkg @ Package.depopts pkg)
          ~init:acc
          ~f:(fun acc (dep : Package_dependency.t) -> loop acc dep.name))
  in
  Package.Name.Set.fold names ~init:Package.Name.Set.empty ~f:(fun name acc ->
    loop acc name)
;;

let package_closure_for_context context names =
  let* workspace = packages ()
  and* candidates = Pkg_sources.candidates context in
  let+ managed =
    Memo.List.map candidates ~f:(fun candidate ->
      let+ dependencies = Pkg_sources.Candidate.dependencies candidate in
      Pkg_sources.Candidate.name candidate, dependencies)
    >>| Package.Name.Map.of_list_exn
  in
  let rec loop acc name =
    if Package.Name.Set.mem acc name
    then acc
    else (
      let acc = Package.Name.Set.add acc name in
      let depends =
        match Package.Name.Map.find workspace name with
        | Some package ->
          Package.depends package @ Package.depopts package
          |> Package.Name.Set.of_list_map ~f:(fun (dep : Package_dependency.t) ->
            dep.name)
        | None ->
          Package.Name.Map.find managed name
          |> Option.value ~default:Package.Name.Set.empty
      in
      Package.Name.Set.fold depends ~init:acc ~f:(fun name acc -> loop acc name))
  in
  Package.Name.Set.fold names ~init:Package.Name.Set.empty ~f:(fun name acc ->
    loop acc name)
;;

let dune_files context =
  let+ t = loaded context in
  t.dune_files
;;

let stanzas_of_package =
  let memo =
    Memo.create
      "stanzas-by-package"
      ~input:(module Context_name)
      (fun context ->
         let+ stanzas = dune_files context in
         Dune_file.fold_static_stanzas
           stanzas
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
         |> Package.Name.Map.map ~f:List.rev)
  in
  fun context package ->
    let+ stanzas = Memo.exec memo context in
    Package.Name.Map.Multi.find stanzas package
;;

let source_tree_dir ~dir =
  match Path.Build.extract_build_context dir with
  | None -> Memo.return None
  | Some (context, _) ->
    let* valid =
      Per_context.valid (Context_name.of_string (Filename.to_string context))
    in
    if not valid
    then Memo.return None
    else
      let* project = find_loaded_project ~dir in
      Loaded_project.source_tree_dir project dir
;;

let find_loaded_project_by_identity ~context ~identity =
  let+ projects = projects_by_output_root context in
  Path.Build.Map.values projects
  |> List.find_exn ~f:(fun project ->
    Loaded_project.Identity.equal identity (Loaded_project.identity project))
;;

let is_vendored ~dir =
  let* project = find_loaded_project ~dir in
  match Build_partition.purpose (Loaded_project.partition project) with
  | Mounted -> Memo.return true
  | Workspace -> Source_tree.is_vendored (Path.Build.drop_build_context_exn dir)
;;

let projects () =
  let+ t = load () in
  t.projects
;;
