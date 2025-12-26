(** Path computation utilities for odoc files *)

open Import

let ( ++ ) = Path.Build.relative

module Doc_mode = Odoc_target.Doc_mode

type sidebar_scope =
  | Per_package of Package.Name.t
  | Global

let odoc_support_dirname = "odoc.support"
let root (context : Context.t) = Path.Build.relative (Context.build_dir context) "_doc"

let index_root ctx mode =
  let subdir =
    match mode with
    | Doc_mode.Local_only -> "_index"
    | Doc_mode.Full -> "_index_full"
  in
  root ctx ++ subdir
;;

let odocs : type a. Context.t -> a Odoc_target.t -> Path.Build.t =
  fun ctx -> function
  | Odoc_target.Lib (pkg, lib) ->
    let lib_name = Lib.name lib in
    root ctx ++ "_odoc" ++ Package.Name.to_string pkg ++ Lib_name.to_string lib_name
  | Odoc_target.Private_lib (lib_unique_name, _) -> root ctx ++ "_odoc" ++ lib_unique_name
  | Odoc_target.Pkg pkg -> root ctx ++ "_odoc" ++ Package.Name.to_string pkg
  | Odoc_target.Toplevel mode -> index_root ctx mode
;;

let html_root ctx mode = root ctx ++ Doc_mode.html_subdir mode
let json_root ctx mode = root ctx ++ Doc_mode.json_subdir mode
let odocl_root ctx = root ctx ++ "_odocl"
let sherlodoc_root ctx = root ctx ++ "_sherlodoc"

let html : type a. Context.t -> Doc_mode.t -> a Odoc_target.t -> Path.Build.t =
  fun ctx mode target ->
  match target with
  | Lib (pkg, lib) ->
    let lib_name = Lib.name lib in
    html_root ctx mode ++ Package.Name.to_string pkg ++ Lib_name.to_string lib_name
  | Private_lib (lib_unique_name, _) -> html_root ctx mode ++ lib_unique_name
  | Pkg pkg -> html_root ctx mode ++ Package.Name.to_string pkg
  | Toplevel _ -> html_root ctx mode
;;

let json : type a. Context.t -> Doc_mode.t -> a Odoc_target.t -> Path.Build.t =
  fun ctx mode target ->
  match target with
  | Lib (pkg, lib) ->
    let lib_name = Lib.name lib in
    json_root ctx mode ++ Package.Name.to_string pkg ++ Lib_name.to_string lib_name
  | Private_lib (lib_unique_name, _) -> json_root ctx mode ++ lib_unique_name
  | Pkg pkg -> json_root ctx mode ++ Package.Name.to_string pkg
  | Toplevel _ -> json_root ctx mode
;;

let odocl : type a. Context.t -> a Odoc_target.t -> Path.Build.t =
  fun ctx -> function
  | Lib (pkg, lib) ->
    let lib_name = Lib.name lib in
    odocl_root ctx ++ Package.Name.to_string pkg ++ Lib_name.to_string lib_name
  | Private_lib (lib_unique_name, _) -> odocl_root ctx ++ lib_unique_name
  | Pkg pkg -> odocl_root ctx ++ Package.Name.to_string pkg
  | Toplevel mode -> index_root ctx mode
;;

let gen_mld_dir ctx pkg = root ctx ++ "_mlds" ++ Package.Name.to_string pkg
let lib_mld_dir ctx pkg lib_name = gen_mld_dir ctx pkg ++ Lib_name.to_string lib_name
let lib_index_mld ctx pkg lib_name = lib_mld_dir ctx pkg lib_name ++ "index.mld"
let odoc_support ctx mode = html_root ctx mode ++ odoc_support_dirname
let odoc_support_for_pkg ctx mode pkg = html_root ctx mode ++ pkg ++ odoc_support_dirname
let toplevel_index_mld ctx mode = index_root ctx mode ++ "index.mld"

let sidebar_root ctx mode =
  let subdir =
    match mode with
    | Doc_mode.Local_only -> "_sidebar"
    | Doc_mode.Full -> "_sidebar_full"
  in
  root ctx ++ subdir
;;

let index_file ctx mode scope =
  match scope with
  | Global -> sidebar_root ctx mode ++ "index.odoc-index"
  | Per_package pkg ->
    sidebar_root ctx mode ++ Package.Name.to_string pkg ++ "index.odoc-index"
;;

let sidebar_file ctx mode scope =
  match scope with
  | Global -> sidebar_root ctx mode ++ "sidebar.odoc-sidebar"
  | Per_package pkg ->
    sidebar_root ctx mode ++ Package.Name.to_string pkg ++ "sidebar.odoc-sidebar"
;;

type output_format =
  | Html
  | Json

let sidebar_json ctx mode scope output_format =
  let root =
    match output_format with
    | Html -> html_root ctx mode
    | Json -> json_root ctx mode
  in
  match scope with
  | Global -> root ++ "sidebar.json"
  | Per_package pkg -> root ++ Package.Name.to_string pkg ++ "sidebar.json"
;;

let remap_file ctx = root ctx ++ "_remap" ++ "remap.txt"
