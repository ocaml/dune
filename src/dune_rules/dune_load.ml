open Import
open Memo.O

type t =
  { dune_files : (Context_name.t -> Dune_file.t list Memo.t) Staged.t Memo.t
  ; packages : Package.t Package.Name.Map.t
  ; projects : Dune_project.t list
  ; projects_by_root : Dune_project.t Path.Source.Map.t
  }

let dune_files t ~context =
  let* f = t.dune_files in
  (Staged.unstage f) context
;;

let packages t = t.packages
let projects t = t.projects
let projects_by_root t = t.projects_by_root

module Projects_and_dune_files =
  Monoid.Product
    (Monoid.Appendable_list (struct
      type t = Dune_project.t
    end))
    (Monoid.Appendable_list (struct
         type t = Path.Source.t * Dune_project.t * Dune_file0.t
       end))

module Source_tree_map_reduce =
  Source_tree.Make_map_reduce_with_progress (Memo) (Projects_and_dune_files)

let load () =
  let open Memo.O in
  let+ projects, dune_files =
    let f dir : Projects_and_dune_files.t Memo.t =
      let path = Source_tree.Dir.path dir in
      let project = Source_tree.Dir.project dir in
      let projects =
        if Path.Source.equal path (Dune_project.root project)
        then Appendable_list.singleton project
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
  let projects = Appendable_list.to_list projects in
  let packages =
    List.fold_left
      projects
      ~init:Package.Name.Map.empty
      ~f:(fun acc (p : Dune_project.t) ->
        Package.Name.Map.merge acc (Dune_project.packages p) ~f:(fun name a b ->
          Option.merge a b ~f:(fun a b ->
            User_error.raise
              [ Pp.textf
                  "Too many opam files for package %S:"
                  (Package.Name.to_string name)
              ; Pp.textf "- %s" (Path.Source.to_string_maybe_quoted (Package.opam_file a))
              ; Pp.textf "- %s" (Path.Source.to_string_maybe_quoted (Package.opam_file b))
              ])))
  in
  let dune_files =
    Memo.Lazy.create ~name:"dune-file-evaluation" (fun () -> Dune_file.eval dune_files)
    |> Memo.Lazy.force
  in
  { dune_files
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
