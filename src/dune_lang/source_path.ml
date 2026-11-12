open Import

module T = struct
  type t =
    | Workspace of Path.Source.t
    | Build of Path.Build.t

  let compare a b =
    match a, b with
    | Workspace a, Workspace b -> Path.Source.compare a b
    | Build a, Build b -> Path.Build.compare a b
    | Workspace _, Build _ -> Lt
    | Build _, Workspace _ -> Gt
  ;;

  let repr =
    Repr.variant
      "source-path"
      [ Repr.case "Workspace" Path.Source.repr ~proj:(function
          | Workspace path -> Some path
          | Build _ -> None)
      ; Repr.case "Build" Path.Build.repr ~proj:(function
          | Build path -> Some path
          | Workspace _ -> None)
      ]
  ;;

  let to_dyn = Repr.to_dyn repr
end

include T

let workspace path = Workspace path
let build path = Build path

let to_path = function
  | Workspace path -> Path.source path
  | Build path -> Path.build path
;;

let to_build_dir ~workspace_build_dir = function
  | Workspace path -> Path.Build.append_source workspace_build_dir path
  | Build path -> path
;;

let equal a b = Ordering.is_eq (compare a b)
let hash t = Path.hash (to_path t)

let relative t path =
  match t with
  | Workspace root -> Workspace (Path.Source.relative root path)
  | Build root -> Build (Path.Build.relative root path)
;;

let relative_fname t path =
  match t with
  | Workspace root -> Workspace (Path.Source.relative_fname root path)
  | Build root -> Build (Path.Build.relative_fname root path)
;;

let append_local t path =
  match t with
  | Workspace root -> Workspace (Path.Source.append_local root path)
  | Build root -> Build (Path.Build.append_local root path)
;;

let basename = function
  | Workspace path -> Path.Source.basename path
  | Build path -> Path.Build.basename path
;;

let parent = function
  | Workspace path -> Option.map (Path.Source.parent path) ~f:workspace
  | Build path -> Option.map (Path.Build.parent path) ~f:build
;;

let parent_exn t = Option.value_exn (parent t)

let descendant t ~of_ =
  match t, of_ with
  | Workspace path, Workspace root ->
    Path.Local.descendant (Path.Source.to_local path) ~of_:(Path.Source.to_local root)
  | Build path, Build root ->
    Path.Local.descendant (Path.Build.local path) ~of_:(Path.Build.local root)
  | Workspace _, Build _ | Build _, Workspace _ -> None
;;

let is_descendant t ~of_ = Option.is_some (descendant t ~of_)
let to_string t = Path.to_string (to_path t)
let to_string_maybe_quoted t = Path.to_string_maybe_quoted (to_path t)

let as_workspace = function
  | Workspace path -> Some path
  | Build _ -> None
;;

let as_workspace_exn = function
  | Workspace path -> path
  | Build path ->
    Code_error.raise
      "Build-backed sources are not supported by this workspace-only operation"
      [ "path", Path.Build.to_dyn path ]
;;

module C = Comparable.Make (T)
module Map = C.Map
module Set = C.Set
