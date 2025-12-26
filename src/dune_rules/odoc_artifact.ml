(** Core artifact type and operations for odoc *)

open Import

let ( ++ ) = Path.Build.relative

type kind =
  | Module : Odoc_target.mod_ * Odoc_target.mod_ Odoc_target.t -> kind
  | Page : Odoc_target.page * Odoc_target.page Odoc_target.t -> kind

type source =
  | Local_source of Path.Build.t
  | Installed_source of { src_path : Path.t }
  | Generated of
      { content : string
      ; output_path : Path.Build.t
      }

type t =
  { kind : kind
  ; source : source
  ; extra_libs : Lib.t list
  ; extra_packages : Package.Name.t list
  }

let get_kind t = t.kind
let extra_libs t = t.extra_libs
let extra_packages t = t.extra_packages

let source_file t =
  match t.source with
  | Local_source path -> Path.build path
  | Installed_source { src_path; _ } -> src_path
  | Generated { output_path; _ } -> Path.build output_path
;;

let generated_content t =
  match t.source with
  | Generated { content; _ } -> Some content
  | Local_source _ | Installed_source _ -> None
;;

let pkg t =
  match t.kind with
  | Module (_, Lib (pkg, _)) -> Some pkg
  | Module (_, Private_lib _) -> None
  | Page (_, Pkg pkg) -> Some pkg
  | Page (_, Toplevel _) -> None
;;

let lib t =
  match t.kind with
  | Module (_, (Lib (_, lib) | Private_lib (_, lib))) -> Some lib
  | Page _ -> None
;;

let lib_name t =
  match t.kind with
  | Module (_, (Lib (_, lib) | Private_lib (_, lib))) -> Lib.name lib
  | Page (_, Pkg pkg) -> Lib_name.of_string (Package.Name.to_string pkg)
  | Page (_, Toplevel _) -> Lib_name.of_string "index"
;;

let odoc_dir ctx t =
  match t.kind with
  | Module (_, target) -> Odoc_paths.odocs ctx target
  | Page (_, target) -> Odoc_paths.odocs ctx target
;;

(* Split hierarchical page name like "foo/baz" into (Some "foo", "baz").
   For non-hierarchical pages like "index", returns (None, "index"). *)
let split_page_name name =
  match String.rsplit2 name ~on:'/' with
  | Some (parent, leaf) -> Some parent, leaf
  | None -> None, name
;;

(* Extract the basename from kind (for pages) or source (for modules).
   For hierarchical pages like "foo/baz", returns just the leaf "baz"
   since the parent path "foo" is already in parent_id. *)
let get_basename t =
  match t.kind, t.source with
  | Page (page, _), _ -> snd (split_page_name page.name)
  | Module (_, _), Local_source src_path ->
    Path.Build.basename src_path |> Filename.remove_extension
  | Module (mod_, _), (Installed_source _ | Generated _) ->
    (* Generated is only used for pages, but handle for exhaustiveness *)
    Module_name.to_string mod_.module_name |> String.uncapitalize_ascii
;;

let odoc_file ctx t =
  let basename = get_basename t in
  match t.kind with
  | Page (page, target) ->
    let base_dir = Odoc_paths.odocs ctx target in
    (* For hierarchical pages like "deprecated/index", include parent path in directory *)
    (match fst (split_page_name page.name) with
     | Some parent_path -> base_dir ++ parent_path ++ ("page-" ^ basename ^ ".odoc")
     | None -> base_dir ++ ("page-" ^ basename ^ ".odoc"))
  | Module (_, target) ->
    let base_dir = Odoc_paths.odocs ctx target in
    base_dir ++ (basename ^ ".odoc")
;;

let odocl_file ctx t =
  let basename = get_basename t in
  match t.kind with
  | Page (page, target) ->
    let base_dir = Odoc_paths.odocl ctx target in
    (* For hierarchical pages like "deprecated/index", include parent path in directory *)
    (match fst (split_page_name page.name) with
     | Some parent_path -> base_dir ++ parent_path ++ ("page-" ^ basename ^ ".odocl")
     | None -> base_dir ++ ("page-" ^ basename ^ ".odocl"))
  | Module (_, target) ->
    let base_dir = Odoc_paths.odocl ctx target in
    base_dir ++ (basename ^ ".odocl")
;;

let output_file ~base ~suffix t =
  let basename = get_basename t in
  match t.kind with
  | Module _ ->
    let dir = base ++ Stdune.String.capitalize basename in
    dir ++ ("index" ^ suffix)
  | Page (page, _) ->
    let path =
      match fst (split_page_name page.name) with
      | Some parent_path -> base ++ parent_path ++ basename
      | None -> base ++ basename
    in
    Path.Build.extend_basename path ~suffix
;;

let html_file ctx mode t =
  let base =
    match t.kind with
    | Module (_, target) -> Odoc_paths.html ctx mode target
    | Page (_, target) -> Odoc_paths.html ctx mode target
  in
  output_file ~base ~suffix:".html" t
;;

let json_file ctx mode t =
  let base =
    match t.kind with
    | Module (_, target) -> Odoc_paths.json ctx mode target
    | Page (_, target) -> Odoc_paths.json ctx mode target
  in
  output_file ~base ~suffix:".html.json" t
;;

let dir_target ~get_base ctx mode t =
  match t.kind with
  | Module (_, target) ->
    let basename = get_basename t in
    let base = get_base ctx mode target in
    Some (base ++ Stdune.String.capitalize basename)
  | Page _ -> None
;;

let html_dir_target ctx mode t = dir_target ~get_base:Odoc_paths.html ctx mode t
let json_dir_target ctx mode t = dir_target ~get_base:Odoc_paths.json ctx mode t

let hidden t =
  match t.kind with
  | Page _ -> false
  | Module _ ->
    let basename = get_basename t in
    String.contains_double_underscore basename
;;

let parent_id t =
  let base_id =
    match t.kind with
    | Module (_, Lib (pkg, lib)) ->
      sprintf "%s/%s" (Package.Name.to_string pkg) (Lib_name.to_string (Lib.name lib))
    | Module (_, Private_lib (lib_unique_name, _)) -> lib_unique_name
    | Page (_, Pkg pkg) -> Package.Name.to_string pkg
    | Page (_, Toplevel _) -> ""
  in
  match t.kind with
  | Module _ -> base_id
  | Page (page, _) ->
    (* For hierarchical pages like "deprecated/index", include parent path without "page-" prefix *)
    (match fst (split_page_name page.name) with
     | Some parent_path -> sprintf "%s/%s" base_id parent_path
     | None -> base_id)
;;

let is_lib_vendored lib =
  let lib_info = Lib.info lib in
  match Lib_info.status lib_info with
  | Installed_private | Installed -> Memo.return false
  | Public _ | Private _ ->
    let src_path = Path.drop_optional_build_context (Lib_info.src_dir lib_info) in
    (match Path.as_in_source_tree src_path with
     | Some src_dir -> Source_tree.is_vendored src_dir
     | None -> Memo.return false)
;;

let should_suppress_output t =
  match t.source with
  | Installed_source _ -> Memo.return true
  | Generated _ -> Memo.return true
  | Local_source _ ->
    (* Check if this is a vendored library *)
    (match t.kind with
     | Module (_, (Lib (_, lib) | Private_lib (_, lib))) -> is_lib_vendored lib
     | Page _ -> Memo.return false)
;;

let create ~kind ~source ~extra_libs ~extra_packages =
  { kind; source; extra_libs; extra_packages }
;;
