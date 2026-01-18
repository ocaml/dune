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
  ; packages : (Package.t * Package.status) list Package.Name.Map.t
  ; projects : Dune_project.t list
  ; projects_by_root : Dune_project.t Path.Source.Map.t
  ; dune_file_by_dir : Dune_file_db.t Per_context.t
  ; mask : Only_packages.t
  ; scopes : Dune_lang.Scope_stanza.t Path.Source.Map.t
  }

type status =
  [ `Vendored
  | `Regular
  ]

module Projects_and_dune_files =
  Monoid.Product3
    (Monoid.Appendable_list (struct
      type t = status * Dune_project.t
    end))
    (Monoid.Appendable_list (struct
         type t = Path.Source.t * Dune_project.t * Source.Dune_file.t
       end))
    (Monoid.Appendable_list (struct
         type t = Path.Source.t * Dune_lang.Scope_stanza.t
       end))

module Source_tree_map_reduce =
  Source_tree.Make_map_reduce_with_progress (Memo) (Projects_and_dune_files)

let load () =
  let status dir =
    match Source_tree.Dir.status dir with
    | Vendored -> `Vendored
    | Normal | Data_only -> `Regular
  in
  let* projects, dune_files, scope_entries =
    let f dir : Projects_and_dune_files.t Memo.t =
      let path = Source_tree.Dir.path dir in
      let project = Source_tree.Dir.project dir in
      let projects =
        if Path.Source.equal path (Dune_project.root project)
        then Appendable_list.singleton (status dir, project)
        else Appendable_list.empty
      in
      let dune_files, scope_entries =
        match Source_tree.Dir.dune_file dir with
        | None -> Appendable_list.empty, Appendable_list.empty
        | Some d ->
          let entries =
            Filename.Map.foldi
              (Source.Dune_file.scope d)
              ~init:Appendable_list.empty
              ~f:(fun subdir (_loc, stanza) acc ->
                let scoped_dir = Path.Source.relative path subdir in
                Appendable_list.cons (scoped_dir, stanza) acc)
          in
          Appendable_list.singleton (path, project, d), entries
      in
      Memo.return (projects, dune_files, scope_entries)
    in
    Source_tree_map_reduce.map_reduce
      ~traverse:Source_dir_status.Set.all
      ~trace_event_name:"Dune load"
      ~f
  in
  let projects = Appendable_list.to_list_rev projects in
  let+ all_packages =
    Memo.List.fold_left
      projects
      ~init:Package.Name.Map.empty
      ~f:(fun acc_packages (status, (project : Dune_project.t)) ->
        let vendored = Poly.equal status `Vendored in
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
            | Some p, Some () -> Some [ p, { Package.enabled = false; vendored } ]
            | Some p, None -> Some [ p, { enabled = true; vendored } ]
            | None, None | None, Some _ -> assert false)
        in
        Package.Name.Map.union acc_packages packages ~f:(fun _name a b -> Some (a @ b)))
  in
  let mask = Only_packages.mask all_packages in
  let packages = Only_packages.filter_packages mask all_packages in
  let projects = List.rev_map projects ~f:snd in
  let dune_files =
    let without_ctx =
      Memo.lazy_ ~name:"dune-files-eval" (fun () ->
        let (_ : Package.Name.t Path.Source.Map.t) =
          match
            Package.Name.Map.values all_packages
            |> List.concat
            |> List.filter_map ~f:(fun (pkg, _) ->
              match Package.exclusive_dir pkg with
              | None -> None
              | Some d -> Some (d, pkg))
            |> Path.Source.Map.of_list_map ~f:(fun ((_loc, d), pkg) ->
              d, Package.name pkg)
          with
          | Ok s -> s
          | Error (dir, ((loc, _), p1), (_, p2)) ->
            let name p = Package.Name.to_string (Package.name p) in
            User_error.raise
              ~loc
              [ Pp.textf
                  "Directory %s cannot belong to package %s"
                  (Path.Source.to_string_maybe_quoted dir)
                  (name p1)
              ; Pp.textf "It already belongs to package %s" (name p2)
              ]
        in
        Dune_file.eval dune_files mask)
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
  ; scopes = Path.Source.Map.of_list_exn (Appendable_list.to_list_rev scope_entries)
  }
;;

let load =
  let memo = Memo.lazy_ ~name:"dune_load" load in
  fun () -> Memo.Lazy.force memo
;;

let find_project ~dir =
  let+ { projects_by_root; _ } = load () in
  Find_closest_source_dir.find_by_dir_exn projects_by_root ~dir
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

let scopes () =
  let+ t = load () in
  t.scopes
;;
