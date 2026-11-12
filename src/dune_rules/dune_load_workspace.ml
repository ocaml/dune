open Import
open Memo.O

type t =
  { dune_files :
      (Source_tree.Rules.Dir.t * Dune_project.t * Source.Dune_file.t) Appendable_list.t
  ; packages : Package.t Package.Name.Map.t
  ; projects : Dune_project.t list
  ; project_dirs : Source_tree.Rules.Dir.t Source_path.Map.t
  ; mask : Only_packages.t
  }

type status =
  [ `Vendored
  | `Regular
  ]

module Projects_and_dune_files =
  Monoid.Product
    (Monoid.Appendable_list (struct
      type t = status * Dune_project.t * Source_tree.Rules.Dir.t
    end))
    (Monoid.Appendable_list (struct
         type t = Source_tree.Rules.Dir.t * Dune_project.t * Source.Dune_file.t
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
      let source_dir = Source_tree.Rules.Dir.source dir in
      let projects =
        if Source_path.equal (Source_path.workspace path) (Dune_project.root project)
        then Appendable_list.singleton (status dir, project, source_dir)
        else Appendable_list.empty
      in
      let dune_files =
        match Source_tree.Dir.dune_file dir with
        | None -> Appendable_list.empty
        | Some d -> Appendable_list.singleton (source_dir, project, d)
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
            |> Memo.List.map ~f:(fun package ->
              Package_enabled.eval package
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
  let project_dirs =
    Source_path.Map.of_list_map_exn projects ~f:(fun (_, project, dir) ->
      Dune_project.root project, dir)
  in
  let projects = List.rev_map projects ~f:(fun (_, project, _) -> project) in
  let (_ : Package.Name.t Source_path.Map.t) =
    match
      Package.Name.Map.values all_packages
      |> List.filter_map ~f:(fun (pkg, _) ->
        match Package.exclusive_dir pkg with
        | None -> None
        | Some d -> Some (d, pkg))
      |> Source_path.Map.of_list_map ~f:(fun ((_loc, d), pkg) -> d, Package.name pkg)
    with
    | Ok s -> s
    | Error (dir, ((loc, _), p1), (_, p2)) ->
      let name p = Package.name p |> Package.Name.to_string in
      User_error.raise
        ~loc
        [ Pp.textf
            "Directory %s cannot belong to package %s"
            (Source_path.to_string_maybe_quoted dir)
            (name p1)
        ; Pp.textf "It already belongs to package %s" (name p2)
        ]
  in
  { dune_files; mask; packages; projects; project_dirs }
;;

let load =
  let memo = Memo.lazy_ ~name:"dune_load" load in
  fun () -> Memo.Lazy.force memo
;;
