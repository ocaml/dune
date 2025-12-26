(** Path computation utilities for odoc files.

    This module computes paths for all odoc-related files in the build tree.
    The documentation lives under [_build/<context>/_doc/] with subdirectories:
    - [_odoc/]: compiled .odoc files
    - [_odocl/]: linked .odocl files
    - [_html/] / [_html_full/]: HTML output
    - [_json/] / [_json_full/]: JSON output
    - [_index/] / [_index_full/]: toplevel index mld, odoc, and odocl
    - [_sidebar/] / [_sidebar_full/]: sidebar index files
    - [_sherlodoc/]: sherlodoc search database
    - [_mlds/]: generated mld files (package/library indices)
    - [_remap/]: URL remap file for Local_only mode *)

open Import
module Doc_mode = Odoc_target.Doc_mode

(** Scope for sidebar generation.

    - [Global]: Single sidebar for all packages
    - [Per_package]: Separate sidebar per package *)
type sidebar_scope =
  | Per_package of Package.Name.t
  | Global

(** Root of all documentation: [_build/<context>/_doc] *)
val root : Context.t -> Path.Build.t

(** Directory for .odoc files for a target. *)
val odocs : Context.t -> 'a Odoc_target.t -> Path.Build.t

(** Root of HTML output for a mode. *)
val html_root : Context.t -> Doc_mode.t -> Path.Build.t

(** Root of JSON output for a mode. *)
val json_root : Context.t -> Doc_mode.t -> Path.Build.t

(** Root directory for .odocl files. *)
val odocl_root : Context.t -> Path.Build.t

(** Root directory for sherlodoc search databases. *)
val sherlodoc_root : Context.t -> Path.Build.t

(** HTML output directory for a target. *)
val html : Context.t -> Doc_mode.t -> 'a Odoc_target.t -> Path.Build.t

(** JSON output directory for a target. *)
val json : Context.t -> Doc_mode.t -> 'a Odoc_target.t -> Path.Build.t

(** Directory for .odocl files for a target. *)
val odocl : Context.t -> 'a Odoc_target.t -> Path.Build.t

(** Directory for generated mld files for a package. *)
val gen_mld_dir : Context.t -> Package.Name.t -> Path.Build.t

(** Path to generated library index.mld. *)
val lib_index_mld : Context.t -> Package.Name.t -> Lib_name.t -> Path.Build.t

(** Path to odoc support files (CSS, JS). *)
val odoc_support : Context.t -> Doc_mode.t -> Path.Build.t

(** Path to odoc support files within a package directory (for per-package mode). *)
val odoc_support_for_pkg : Context.t -> Doc_mode.t -> string -> Path.Build.t

(** Path to toplevel index.mld (generated). *)
val toplevel_index_mld : Context.t -> Doc_mode.t -> Path.Build.t

(** Path to sidebar index file (.odoc-index). *)
val index_file : Context.t -> Doc_mode.t -> sidebar_scope -> Path.Build.t

(** Path to binary sidebar file (.odoc-sidebar). *)
val sidebar_file : Context.t -> Doc_mode.t -> sidebar_scope -> Path.Build.t

(** Output format for sidebar JSON generation. *)
type output_format =
  | Html
  | Json

(** Path to sidebar.json for web-based navigation. *)
val sidebar_json
  :  Context.t
  -> Doc_mode.t
  -> sidebar_scope
  -> output_format
  -> Path.Build.t

(** Path to the remap.txt file for Local_only mode URL remapping. *)
val remap_file : Context.t -> Path.Build.t
