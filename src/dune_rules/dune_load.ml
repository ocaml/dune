open Import
open Memo.O

module Dune_file_db = struct
  type t = Dune_file.t Path.Build.Map.t

  let make all =
    Path.Build.Map.of_list_map_exn all ~f:(fun dune_file ->
      Dune_file.output_dir dune_file, dune_file)
  ;;

  let per_context dune_files =
    Per_context.create_by_name ~name:"dune-file-db" (fun ctx ->
      Memo.lazy_ ~name:"dune-file-db" (fun () -> dune_files ctx >>| make)
      |> Memo.Lazy.force)
  ;;
end

type t =
  { dune_files : Dune_file.t list Per_context.t
  ; packages : Package.t Package.Name.Map.t
  ; projects : Dune_project.t list
  ; projects_by_root : Dune_project.t Path.Source.Map.t
  ; dune_file_by_dir : Dune_file_db.t Per_context.t
  ; mask : Only_packages.t
  }

type status =
  [ `Vendored
  | `Regular
  ]

module Projects_and_dune_files =
  Monoid.Product
    (Monoid.Appendable_list (struct
      type t = status * Dune_project.t
    end))
    (Monoid.Appendable_list (struct
         type t = Path.Source.t * Dune_project.t * Source.Dune_file.t
       end))

module Source_tree_map_reduce =
  Source_tree.Make_map_reduce_with_progress (Memo) (Projects_and_dune_files)

let load () =
  let status dir =
    match Source_tree.Dir.status dir with
    | Vendored -> `Vendored
    | Normal | Data_only -> `Regular
  in
  let* projects, dune_files =
    let f dir : Projects_and_dune_files.t Memo.t =
      let path = Source_tree.Dir.path dir in
      let project = Source_tree.Dir.project dir in
      let projects =
        if Source_path.equal (Source_path.workspace path) (Dune_project.root project)
        then Appendable_list.singleton (status dir, project)
        else Appendable_list.empty
      in
      let dune_files =
        match Source_tree.Dir.dune_file dir with
        | None -> Appendable_list.empty
        | Some d -> Appendable_list.singleton (path, project, d)
      in
      Memo.return (projects, dune_files)
    in
    Source_tree_map_reduce.map_reduce
      ~traverse:Source_dir_status.Set.all
      ~trace_event_name:"Dune load"
      ~f
  in
  let projects = Appendable_list.to_list_rev projects in
  let+ all_packages, vendored_packages =
    Memo.List.fold_left
      projects
      ~init:(Package.Name.Map.empty, Package.Name.Set.empty)
      ~f:(fun (acc_packages, vendored) (status, (project : Dune_project.t)) ->
        let+ packages =
          let packages = Dune_project.including_hidden_packages project in
          let+ disabled =
            Package.Name.Map.values packages
            |> List.filter_map ~f:(fun package ->
              Package.enabled_if package |> Option.map ~f:(fun expr -> package, expr))
            |> Memo.List.map ~f:(fun (package, expr) ->
              Blang_expand.eval
                expr
                ~dir:Path.root (* This value is irrelevant *)
                ~f:(fun ~source:_ pform ->
                  match pform with
                  | Var (Os v) -> Lock_dir.Sys_vars.(os_values poll v)
                  | Var Architecture ->
                    let+ arch = Memo.Lazy.force Lock_dir.Sys_vars.poll.arch in
                    [ Value.String (Option.value ~default:"" arch) ]
                  | _ -> assert false)
              >>| function
              | true -> None
              | false -> Some package)
            >>| List.filter_opt
            >>| Package.Name.Map.of_list_map_exn ~f:(fun pkg -> Package.name pkg, ())
          in
          Package.Name.Map.merge packages disabled ~f:(fun _key package disabled ->
            match package, disabled with
            | Some p, Some () -> Some (p, `Disabled)
            | Some p, None -> Some (p, `Enabled)
            | None, None | None, Some _ -> assert false)
        in
        let vendored =
          match status with
          | `Regular -> vendored
          | `Vendored ->
            Package.Name.Set.of_keys packages |> Package.Name.Set.union vendored
        in
        let acc_packages =
          Package.Name.Map.union acc_packages packages ~f:(fun name (a, _) (b, _) ->
            User_error.raise
              [ Pp.textf
                  "The package %S is defined more than once:"
                  (Package.Name.to_string name)
              ; Pp.textf "- %s" (Loc.to_file_colon_line (Package.loc a))
              ; Pp.textf "- %s" (Loc.to_file_colon_line (Package.loc b))
              ])
        in
        acc_packages, vendored)
  in
  let mask = Only_packages.mask all_packages ~vendored:vendored_packages in
  let packages =
    Package.Name.Map.map ~f:fst all_packages |> Only_packages.filter_packages mask
  in
  let projects = List.rev_map projects ~f:snd in
  let dune_files =
    let without_ctx =
      Memo.lazy_ ~name:"dune-files-eval" (fun () ->
        let (_ : Package.Name.t Source_path.Map.t) =
          match
            Package.Name.Map.values all_packages
            |> List.filter_map ~f:(fun (pkg, _) ->
              match Package.exclusive_dir pkg with
              | None -> None
              | Some d -> Some (d, pkg))
            |> Source_path.Map.of_list_map ~f:(fun ((_loc, d), pkg) ->
              d, Package.name pkg)
          with
          | Ok s -> s
          | Error (dir, ((loc, _), p1), (_, p2)) ->
            let name p = Package.Name.to_string (Package.name p) in
            User_error.raise
              ~loc
              [ Pp.textf
                  "Directory %s cannot belong to package %s"
                  (Source_path.to_string_maybe_quoted dir)
                  (name p1)
              ; Pp.textf "It already belongs to package %s" (name p2)
              ]
        in
        Dune_file.eval dune_files mask)
    in
    Per_context.create_by_name ~name:"dune-files" (fun ctx ->
      Memo.Lazy.create ~name:"dune-files-for-context" (fun () ->
        let* f = Memo.Lazy.force without_ctx in
        f ctx)
      |> Memo.Lazy.force)
    |> Staged.unstage
  in
  let dune_file_by_dir = Dune_file_db.per_context dune_files |> Staged.unstage in
  { dune_files
  ; mask
  ; dune_file_by_dir
  ; packages
  ; projects
  ; projects_by_root =
      Path.Source.Map.of_list_map_exn projects ~f:(fun project ->
        Dune_project.root project |> Source_path.as_workspace_exn, project)
  }
;;

let load =
  let memo = Memo.lazy_ ~name:"dune_load" load in
  fun () -> Memo.Lazy.force memo
;;

let projects_by_output_root =
  let by_context =
    Per_context.create_by_name ~name:"workspace-loaded-projects" (fun context_name ->
      Memo.lazy_ ~name:"workspace-loaded-projects-for-context" (fun () ->
        let* { projects_by_root; _ } = load () in
        let+ context = Context.DB.get context_name in
        let partition = Build_partition.workspace context in
        Path.Source.Map.to_list_map projects_by_root ~f:(fun root project ->
          let output_root = Path.Build.append_source (Context.build_dir context) root in
          let loaded_project =
            Loaded_project.create
              ~project
              ~identity:(Loaded_project.Identity.workspace root)
              ~source_root:(Dune_project.root project)
              ~partition
              ~output_root
          in
          output_root, loaded_project)
        |> Path.Build.Map.of_list_exn)
      |> Memo.Lazy.force)
    |> Staged.unstage
  in
  fun context -> by_context context
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
      let* { dune_file_by_dir; _ } = load () in
      let+ map = dune_file_by_dir ctx in
      Path.Build.Map.find map dir)
;;

let mask () =
  let+ { mask; _ } = load () in
  mask
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

let dune_files context =
  let* t = load () in
  t.dune_files context
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

let projects_by_root () =
  let+ t = load () in
  t.projects_by_root
;;

let projects () =
  let+ t = load () in
  t.projects
;;
