open Import

type t =
  { project : Dune_project.t
  ; owner : Package.Name.t option
  ; source_tree_root : Source_tree.Dir.t
  ; build_dir : Path.Build.t
  ; partition_root : Path.Build.t
  ; output_root : Path.Build.t
  ; stanza_mask : Package.Name.Set.t option
  }

let create ~project ~owner ~source_tree_root ~build_dir ~partition_root ~stanza_mask =
  { project
  ; owner
  ; source_tree_root
  ; build_dir
  ; partition_root
  ; output_root =
      Source_tree.Dir.source_path source_tree_root |> Path.Build.append_local build_dir
  ; stanza_mask
  }
;;

let project t = t.project
let source_root t = Source_tree.Dir.source_path t.source_tree_root
let build_dir t = t.build_dir
let owner t = t.owner
let partition_root t = t.partition_root
let output_root t = t.output_root
let stanza_mask t = t.stanza_mask
let visible_packages t = Option.bind t.owner ~f:(fun _ -> t.stanza_mask)

let vendored_warnings t =
  match t.owner with
  | Some _ -> true
  | None ->
    (match Source_tree.Dir.status t.source_tree_root with
     | Vendored -> true
     | Normal | Data_only -> false)
;;

let visible_project t =
  match visible_packages t with
  | None -> t.project
  | Some packages ->
    Dune_project.filter_packages t.project ~f:(Package.Name.Set.mem packages)
;;

let packages t =
  let packages = Dune_project.including_hidden_packages t.project in
  match visible_packages t with
  | None -> packages
  | Some visible ->
    Package.Name.Map.filteri packages ~f:(fun name _ -> Package.Name.Set.mem visible name)
;;

let output_path t source_path = Path.Build.append_local t.build_dir source_path

let source_tree_dir t output_path =
  Source_tree.Dir.find_dir t.source_tree_root (Dune_project.dir_in_context output_path)
;;
