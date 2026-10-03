open Import

module Identity = struct
  type t =
    | Workspace of Path.Source.t
    | Mounted of
        { package : Package.Name.t
        ; project_root : Path.Local.t
        }

  let workspace root = Workspace root
  let mounted ~package ~project_root = Mounted { package; project_root }

  let equal a b =
    match a, b with
    | Workspace a, Workspace b -> Path.Source.equal a b
    | Mounted a, Mounted b ->
      Package.Name.equal a.package b.package
      && Path.Local.equal a.project_root b.project_root
    | Workspace _, Mounted _ | Mounted _, Workspace _ -> false
  ;;

  let repr =
    Repr.variant
      "loaded-project-identity"
      [ Repr.case "Workspace" Path.Source.repr ~proj:(function
          | Workspace root -> Some root
          | Mounted _ -> None)
      ; Repr.case
          "Mounted"
          Repr.(pair Package.Name.repr Path.Local.repr)
          ~proj:(function
            | Mounted { package; project_root } -> Some (package, project_root)
            | Workspace _ -> None)
      ]
  ;;

  let digest t = Dune_digest.repr repr t

  let to_dyn = function
    | Workspace root -> Dyn.variant "Workspace" [ Path.Source.to_dyn root ]
    | Mounted { package; project_root } ->
      Dyn.variant
        "Mounted"
        [ Dyn.record
            [ "package", Package.Name.to_dyn package
            ; "project_root", Path.Local.to_dyn project_root
            ]
        ]
  ;;
end

type t =
  { project : Dune_project.t
  ; identity : Identity.t
  ; source_root : Source_path.t
  ; source_tree_root : Source_tree.Rules.Dir.t
  ; partition : Build_partition.t
  ; output_root : Path.Build.t
  ; visible_packages : Package.Name.Set.t option
  ; stanza_mask : Package.Name.Set.t option
  }

let create
      ~project
      ~identity
      ~source_root
      ~source_tree_root
      ~partition
      ~output_root
      ~visible_packages
      ~stanza_mask
  =
  if
    not
      (Source_path.equal source_root (Source_tree.Rules.Dir.source_path source_tree_root))
  then
    Code_error.raise
      "Loaded project source root and source-tree root disagree"
      [ "source_root", Source_path.to_dyn source_root
      ; "source_tree_root", Source_tree.Rules.Dir.to_dyn source_tree_root
      ];
  if
    not
      (Path.is_descendant
         (Path.build output_root)
         ~of_:(Path.build (Build_partition.output_root partition)))
  then
    Code_error.raise
      "Loaded_project output root is outside its build partition"
      [ "output_root", Path.Build.to_dyn output_root
      ; "partition", Build_partition.to_dyn partition
      ];
  { project
  ; identity
  ; source_root
  ; source_tree_root
  ; partition
  ; output_root
  ; visible_packages
  ; stanza_mask
  }
;;

let project t = t.project
let identity t = t.identity
let source_root t = t.source_root
let source_tree_root t = t.source_tree_root
let partition t = t.partition
let output_root t = t.output_root
let visible_packages t = t.visible_packages
let stanza_mask t = t.stanza_mask

let hide_packages t =
  { t with
    visible_packages = Some Package.Name.Set.empty
  ; stanza_mask = Some Package.Name.Set.empty
  }
;;

let owner t =
  match t.identity with
  | Identity.Workspace _ -> None
  | Mounted { package; _ } ->
    (match Source_tree.Rules.Dir.status t.source_tree_root with
     | Vendored
       when not
              (Package.Name.Map.mem
                 (Dune_project.including_hidden_packages t.project)
                 package) -> None
     | Vendored | Normal | Data_only -> Some package)
;;

let packages t =
  let packages = Dune_project.including_hidden_packages t.project in
  match t.visible_packages with
  | None -> packages
  | Some visible ->
    Package.Name.Map.filteri packages ~f:(fun name _ -> Package.Name.Set.mem visible name)
;;

let output_path t source_path =
  Source_path.descendant source_path ~of_:t.source_root
  |> Option.map ~f:(Path.Build.append_local t.output_root)
;;

let source_path t output_path =
  Path.drop_prefix (Path.build output_path) ~prefix:(Path.build t.output_root)
  |> Option.map ~f:(Source_path.append_local t.source_root)
;;

let source_tree_dir t output_path =
  match Path.drop_prefix (Path.build output_path) ~prefix:(Path.build t.output_root) with
  | None -> Memo.return None
  | Some local -> Source_tree.Rules.Dir.find_dir t.source_tree_root local
;;

let to_dyn t =
  Dyn.record
    [ "identity", Identity.to_dyn t.identity
    ; "source_root", Source_path.to_dyn t.source_root
    ; "source_tree_root", Source_tree.Rules.Dir.to_dyn t.source_tree_root
    ; "partition", Build_partition.to_dyn t.partition
    ; "output_root", Path.Build.to_dyn t.output_root
    ; "visible_packages", Dyn.option Package.Name.Set.to_dyn t.visible_packages
    ; "stanza_mask", Dyn.option Package.Name.Set.to_dyn t.stanza_mask
    ]
;;
