open Import
open Memo.O

type t =
  { dune_files : (Source_tree.Dir.t * Source.Dune_file.t) Appendable_list.t
  ; packages : Package.t Package.Name.Map.t
  ; projects : (Dune_project.t * Source_tree.Dir.t) list
  ; mask : Only_packages.t
  }

type status =
  [ `Vendored
  | `Regular
  ]

let package_enabled package =
  match Package.enabled_if package with
  | None -> Memo.return true
  | Some expr ->
    Blang_expand.eval expr ~dir:Path.root ~f:(fun ~source:_ pform ->
      match pform with
      | Var (Os v) -> Lock_dir.Sys_vars.(os_values poll v)
      | Var Architecture ->
        let+ arch = Memo.Lazy.force Lock_dir.Sys_vars.poll.arch in
        [ Value.String (Option.value ~default:"" arch) ]
      | pform ->
        Code_error.raise
          "Dune_load.package_enabled: variable not allowed"
          [ "variable", Pform.to_dyn pform ])
;;

module Projects_and_dune_files =
  Monoid.Product
    (Monoid.Appendable_list (struct
      type t = status * Dune_project.t * Source_tree.Dir.t
    end))
    (Monoid.Appendable_list (struct
         type t = Source_tree.Dir.t * Source.Dune_file.t
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
      (match Path.Source.split_first_component path with
       | Some (name, _) when Filename.equal name Filename.pkg_dir_basename ->
         User_error.raise
           ~loc:(Loc.in_dir (Path.source path))
           [ Pp.textf
               "The directory %s is reserved for locked packages and cannot be part of \
                the workspace."
               (Filename.to_string Filename.pkg_dir_basename)
           ]
       | Some _ | None -> ());
      let project = Source_tree.Dir.project dir in
      let projects =
        if Path.Local.equal (Path.Source.to_local path) (Dune_project.root project)
        then Appendable_list.singleton (status dir, project, dir)
        else Appendable_list.empty
      in
      let dune_files =
        match Source_tree.Dir.dune_file dir with
        | None -> Appendable_list.empty
        | Some d -> Appendable_list.singleton (dir, d)
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
      ~f:(fun (acc_packages, vendored) (status, (project : Dune_project.t), _) ->
        let+ packages =
          let packages = Dune_project.including_hidden_packages project in
          let+ disabled =
            Package.Name.Map.values packages
            |> Memo.List.filter ~f:(fun package -> package_enabled package >>| not)
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
  let projects = List.rev_map projects ~f:(fun (_, project, dir) -> project, dir) in
  let (_ : Package.Name.t Path.Local.Map.t) =
    match
      Package.Name.Map.values all_packages
      |> List.filter_map ~f:(fun (pkg, _) ->
        match Package.exclusive_dir pkg with
        | None -> None
        | Some d -> Some (d, pkg))
      |> Path.Local.Map.of_list_map ~f:(fun ((_loc, d), pkg) -> d, Package.name pkg)
    with
    | Ok s -> s
    | Error (dir, ((loc, _), p1), (_, p2)) ->
      let name p = Package.Name.to_string (Package.name p) in
      User_error.raise
        ~loc
        [ Pp.textf
            "Directory %s cannot belong to package %s"
            (Path.Local.to_string_maybe_quoted dir)
            (name p1)
        ; Pp.textf "It already belongs to package %s" (name p2)
        ]
  in
  { dune_files; mask; packages; projects }
;;

let load =
  let memo = Memo.lazy_ ~name:"dune_load" load in
  fun () -> Memo.Lazy.force memo
;;

let mask () =
  let+ { mask; _ } = load () in
  mask
;;

let packages () =
  let+ { packages; _ } = load () in
  packages
;;

let projects () =
  let+ t = load () in
  List.map t.projects ~f:fst
;;
