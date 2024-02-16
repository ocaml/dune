open Import
open Memo.O

module Dune_file_db = struct
  type t = Dune_file.t Path.Source.Map.t

  let make all =
    Path.Source.Map.of_list_map_exn all ~f:(fun dune_file ->
      Dune_file.dir dune_file, dune_file)
  ;;

  let per_context dune_files =
    Per_context.create_by_name ~name:"dune-file-db" (fun ctx ->
      Memo.lazy_ (fun () -> dune_files ctx >>| make) |> Memo.Lazy.force)
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
         type t = Path.Source.t * Dune_project.t * Dune_file0.t
       end))

module Source_tree_map_reduce =
  Source_tree.Make_map_reduce_with_progress (Memo) (Projects_and_dune_files)

let load () =
  let open Memo.O in
  let status dir =
    match Source_tree.Dir.status dir with
    | Vendored -> `Vendored
    | Normal | Data_only -> `Regular
  in
  let+ projects, dune_files =
    let f dir : Projects_and_dune_files.t Memo.t =
      let path = Source_tree.Dir.path dir in
      let project = Source_tree.Dir.project dir in
      let projects =
        if Path.Source.equal path (Dune_project.root project)
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
    Source_tree_map_reduce.map_reduce ~traverse:Source_dir_status.Set.all ~f
  in
  let projects = Appendable_list.to_list_rev projects in
  let packages, vendored_packages =
    List.fold_left
      projects
      ~init:(Package.Name.Map.empty, Package.Name.Set.empty)
      ~f:(fun (acc_packages, vendored) (status, (project : Dune_project.t)) ->
        let packages = Dune_project.including_hidden_packages project in
        let vendored =
          match status with
          | `Regular -> vendored
          | `Vendored ->
            Package.Name.Set.of_keys packages |> Package.Name.Set.union vendored
        in
        let acc_packages =
          Package.Name.Map.union acc_packages packages ~f:(fun name a b ->
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
  let mask = Only_packages.mask packages ~vendored:vendored_packages in
  let packages = Only_packages.filter_packages mask packages in
  let projects = List.rev_map projects ~f:snd in
  let dune_files =
    let without_ctx =
      Memo.lazy_ ~name:"dune-files-eval" (fun () -> Dune_file.eval dune_files mask)
    in
    Per_context.create_by_name ~name:"dune-files" (fun ctx ->
      Memo.Lazy.create (fun () ->
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
        Dune_project.root project, project)
  }
;;

let load =
  let memo = Memo.lazy_ ~name:"dune_load" load in
  fun () -> Memo.Lazy.force memo
;;

let find_project ~dir =
  let+ { projects_by_root; _ } = load () in
  Find_closest_source_dir.find_by_dir projects_by_root ~dir
;;

let stanzas_in_dir dir =
  if Path.Build.is_root dir
  then Memo.return None
  else (
    match Install.Context.of_path dir with
    | None -> Memo.return None
    | Some ctx ->
      let dir = Path.Build.drop_build_context_exn dir in
      let* { dune_file_by_dir; _ } = load () in
      let+ map = dune_file_by_dir ctx in
      Path.Source.Map.find map dir)
;;

let mask () =
  let+ { mask; _ } = load () in
  mask
;;

let packages () =
  let+ { packages; _ } = load () in
  packages
;;

let dune_files context =
  let* t = load () in
  t.dune_files context
;;

let projects_by_root () =
  let+ t = load () in
  t.projects_by_root
;;

let projects () =
  let+ t = load () in
  t.projects
;;
