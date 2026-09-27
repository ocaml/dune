open Import

module Identity = struct
  type t =
    | Workspace of Path.Source.t
    | Mounted of
        { lock : Dune_digest.t
        ; package : Package.Name.t
        ; project_root : Path.Local.t
        }

  let workspace root = Workspace root
  let mounted ~lock ~package ~project_root = Mounted { lock; package; project_root }

  let to_dyn = function
    | Workspace root -> Dyn.variant "Workspace" [ Path.Source.to_dyn root ]
    | Mounted { lock; package; project_root } ->
      Dyn.variant
        "Mounted"
        [ Dyn.record
            [ "lock", Dune_digest.to_dyn lock
            ; "package", Package.Name.to_dyn package
            ; "project_root", Path.Local.to_dyn project_root
            ]
        ]
  ;;
end

type t =
  { project : Dune_project.t
  ; identity : Identity.t
  ; source_root : Source_path.t
  ; partition : Build_partition.t
  ; output_root : Path.Build.t
  }

let create ~project ~identity ~source_root ~partition ~output_root =
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
  { project; identity; source_root; partition; output_root }
;;

let project t = t.project
let identity t = t.identity
let source_root t = t.source_root
let partition t = t.partition
let output_root t = t.output_root

let to_dyn t =
  Dyn.record
    [ "identity", Identity.to_dyn t.identity
    ; "source_root", Source_path.to_dyn t.source_root
    ; "partition", Build_partition.to_dyn t.partition
    ; "output_root", Path.Build.to_dyn t.output_root
    ]
;;
