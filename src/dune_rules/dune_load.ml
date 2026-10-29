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

let load_mounted context =
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
                     let vendored =
                       match Source_tree.Rules.Dir.status source_tree_root with
                       | Vendored -> true
                       | Normal | Data_only -> false
                     in
                     Dune_project.including_hidden_packages project
                     |> Package.Name.Map.values
                     |> Memo.List.filter ~f:(fun package ->
                       if vendored || Package.Name.equal owner (Package.name package)
                       then Package_enabled.eval package
                       else Memo.return false)
                     >>| Package.Name.Set.of_list_map ~f:Package.name
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
                     ~stanza_mask:(Some visible_packages)
                     ~vendored_warnings:true
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
           let* eval = Dune_file.eval files in
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

let rec same_source_tree left right =
  let module Dir = Source_tree.Rules.Dir in
  let same_status =
    match Dir.status left, Dir.status right with
    | Normal, Normal | Vendored, Vendored | Data_only, Data_only -> true
    | _ -> false
  in
  if
    (not same_status)
    || (not (Filename.Array.Set.equal (Dir.filenames left) (Dir.filenames right)))
    || not (Filename.Array.Set.equal (Dir.sub_dir_names left) (Dir.sub_dir_names right))
  then Memo.return false
  else
    let* same_files =
      Filename.Array.Set.to_list (Dir.filenames left)
      |> Memo.List.for_all ~f:(fun name ->
        let name = Path.Local.relative_fname Path.Local.root name in
        let+ left = Dir.file left name |> Source_tree.Rules.File.materialization
        and+ right = Dir.file right name |> Source_tree.Rules.File.materialization in
        match left, right with
        | Some (Copy left), Some (Copy right) -> Path.equal left right
        | Some (Write left), Some (Write right) ->
          String.equal left.contents right.contents
          && Bool.equal left.executable right.executable
          && List.equal Path.equal left.dependencies right.dependencies
        | None, None -> true
        | _ -> false)
    in
    if not same_files
    then Memo.return false
    else
      Filename.Array.Set.to_list (Dir.sub_dir_names left)
      |> Memo.List.for_all ~f:(fun name ->
        let* left =
          Filename.Array.Map.find (Dir.sub_dirs left) name
          |> Option.value_exn
          |> Dir.sub_dir_as_t
        and* right =
          Filename.Array.Map.find (Dir.sub_dirs right) name
          |> Option.value_exn
          |> Dir.sub_dir_as_t
        in
        same_source_tree left right)
;;

(* Compare the complete prepared views: an overlay outside the vendored project
   can change its inherited environment too. Sharing must not discard it. *)
let share_vendored_projects projects files =
  let roots =
    Path.Build.Map.of_list_map_exn projects ~f:(fun project ->
      Loaded_project.output_root project, Loaded_project.source_tree_root project)
  in
  let+ _, hidden, projects =
    Memo.List.fold_left
      projects
      ~init:(Path.Map.empty, Path.Build.Set.empty, [])
      ~f:(fun (seen, hidden, projects) project ->
        let dir = Loaded_project.source_tree_root project in
        match Source_tree.Rules.Dir.status dir with
        | Normal | Data_only -> Memo.return (seen, hidden, project :: projects)
        | Vendored ->
          let backing = Source_tree.Rules.Dir.backing_path dir in
          let root =
            Loaded_project.partition project
            |> Build_partition.output_root
            |> Path.Build.Map.find_exn roots
          in
          let* duplicate =
            match Path.Map.find seen backing with
            | None -> Memo.return false
            | Some previous -> same_source_tree previous root
          in
          if duplicate
          then
            Memo.return
              ( seen
              , Path.Build.Set.add hidden (Loaded_project.output_root project)
              , Loaded_project.hide_packages project :: projects )
          else Memo.return (Path.Map.set seen backing root, hidden, project :: projects))
  in
  ( List.rev projects
  , List.filter files ~f:(fun file ->
      let project = Dune_file.loaded_dir file |> Loaded_dir.project in
      not (Path.Build.Set.mem hidden (Loaded_project.output_root project))) )
;;

let loaded =
  let by_context =
    Per_context.create_by_name ~name:"workspace-loaded-projects" (fun context_name ->
      Memo.lazy_ ~name:"workspace-loaded-projects-for-context" (fun () ->
        let* workspace = load ()
        and* context = Context.DB.get context_name in
        let partition = Build_partition.workspace context in
        let stanza_mask =
          match Only_packages.enumerate workspace.mask with
          | `All -> None
          | `Set names -> Some names
        in
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
              ~visible_packages:None
              ~stanza_mask
              ~vendored_warnings:false)
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
        let* mounted_projects, mounted_files, mounted_errors = load_mounted context in
        let* () =
          let local_packages =
            List.fold_left
              mounted_projects
              ~init:Package.Name.Map.empty
              ~f:(fun packages project ->
                match Loaded_project.owner project with
                | Some _ -> packages
                | None ->
                  Package.Name.Map.foldi
                    (Loaded_project.packages project)
                    ~init:packages
                    ~f:(fun name package packages ->
                      Package.Name.Map.set packages name (Package.loc package)))
          in
          if Package.Name.Map.is_empty local_packages
          then Memo.return ()
          else
            let+ lock_dir = Lock_dir.get_exn context_name in
            Dune_pkg.Package_universe.check_package_names lock_dir ~local_packages
        in
        let* mounted_projects, mounted_files =
          share_vendored_projects (List.rev mounted_projects) mounted_files
        in
        let projects = projects @ mounted_projects in
        let (_ : Package.t Package.Name.Map.t) =
          List.fold_left projects ~init:Package.Name.Map.empty ~f:(fun packages project ->
            Package.Name.Map.union
              packages
              (Loaded_project.packages project)
              ~f:(fun name first second ->
                User_error.raise
                  [ Pp.textf
                      "The package %S is defined more than once:"
                      (Package.Name.to_string name)
                  ; Pp.textf "- %s" (Loc.to_file_colon_line (Package.loc first))
                  ; Pp.textf "- %s" (Loc.to_file_colon_line (Package.loc second))
                  ]))
        in
        let* eval = Dune_file.eval source_dune_files in
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
  let* workspace = load () in
  let+ projects = projects_by_output_root context in
  Path.Build.Map.fold projects ~init:workspace.packages ~f:(fun project packages ->
    match Build_partition.purpose (Loaded_project.partition project) with
    | Workspace -> packages
    | Mounted -> Package.Name.Map.union_exn packages (Loaded_project.packages project))
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
  let+ project = find_loaded_project ~dir in
  match Loaded_project.stanza_mask project with
  | Some packages -> `Set packages
  | None -> `All
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
  let* source_packages = packages_for_context context
  and* candidates = Pkg_sources.candidates context in
  let+ managed =
    Memo.List.map candidates ~f:(fun candidate ->
      let+ dependencies = Pkg_sources.Candidate.dependencies candidate in
      ( Pkg_sources.Candidate.name candidate
      , Package.Name.Set.remove dependencies Dune_pkg.Dune_dep.name ))
    >>| Package.Name.Map.of_list_exn
  in
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
           | None -> Package.Name.Set.empty
           | Some package ->
             Package.depends package @ Package.depopts package
             |> Package.Name.Set.of_list_map ~f:(fun (dep : Package_dependency.t) ->
               dep.name))
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

let source_tree_root ~dir =
  match Path.Build.extract_build_context dir with
  | None -> Memo.return None
  | Some (context, _) ->
    let context = Context_name.of_string (Filename.to_string context) in
    let* valid = Per_context.valid context in
    if not valid
    then Memo.return None
    else (
      match Pkg_sources.package_of_artifact_path dir with
      | None ->
        Source_tree.root () >>| fun root -> Some (Source_tree.Rules.Dir.source root)
      | Some package ->
        let+ mounted = Pkg_sources.find_mounted context package in
        Option.bind mounted ~f:Pkg_sources.Mounted.tree
        |> Option.map ~f:Source_tree.Rules.Mounted.root)
;;

let source_tree_dir ~dir =
  let* root = source_tree_root ~dir in
  match root with
  | None -> Memo.return None
  | Some root ->
    let output_root =
      Source_tree.Rules.Dir.source_path root
      |> Source_path.to_build_dir
           ~workspace_build_dir:(Context_name.build_dir (context_of_dir dir))
    in
    let local = Path.drop_prefix (Path.build dir) ~prefix:(Path.build output_root) in
    (match local with
     | None -> Memo.return None
     | Some local -> Source_tree.Rules.Dir.find_dir root local)
;;

let find_loaded_project_by_identity ~context ~identity =
  let+ projects = projects_by_output_root context in
  Path.Build.Map.values projects
  |> List.find_exn ~f:(fun project ->
    Loaded_project.Identity.equal identity (Loaded_project.identity project))
;;

let vendored_warnings ~dir =
  let* project = find_loaded_project ~dir in
  if Loaded_project.vendored_warnings project
  then Memo.return true
  else (
    let rec loop dir =
      let* source_dir = Loaded_project.source_tree_dir project dir in
      match source_dir with
      | Some dir ->
        Memo.return
          (match Source_tree.Rules.Dir.status dir with
           | Vendored -> true
           | Normal | Data_only -> false)
      | None ->
        (match Path.Build.parent dir with
         | None -> Memo.return false
         | Some dir -> loop dir)
    in
    loop dir)
;;

let projects () =
  let+ t = load () in
  t.projects
;;
