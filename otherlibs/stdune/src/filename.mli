(** Represent a non-empty path component.

    A path component is just a non-empty string without a '/' character. It
    cannot be ["."] or [".."]. *)

val current_dir_name : string
val parent_dir_name : string
val dir_sep : string
val concat : string -> string -> string
val is_relative : string -> bool
val check_suffix : string -> string -> bool
val basename : string -> string
val dirname : string -> string
val get_temp_dir_name : unit -> string
val quote : string -> string

type t

val of_string : string -> t option
val of_string_exn : string -> t
val to_string : t -> string

(** [append dir fn] is [concat dir (to_string fn)]. *)
val append : string -> t -> string

val to_dyn : t -> Dyn.t
val pp : t -> 'a Pp.t
val remove_extension : t -> t
val repr : t Repr.t

module L : sig
  val to_string : t list -> string list
end

module Extension : sig
  type filename := t
  type t

  val corrected : t
  val ml : t
  val mli : t
  val mllib : t
  val mll : t
  val vo : t
  val vos : t
  val mly : t
  val theory_d : t
  val map : t
  val odocl : t
  val deps : t
  val cma : t
  val cmxa : t
  val cmxs : t
  val cmi : t
  val cmx : t
  val cmj : t
  val cmo : t
  val cmi_dump : t
  val cmo_dump : t
  val exe : t
  val expected : t
  val bc : t
  val bc_exe : t
  val ml_gen : t
  val cmt : t
  val cmti : t
  val cms : t
  val cmsi : t
  val odoc : t
  val opam : t
  val h : t
  val d : t
  val js : t
  val mlg : t
  val json : t
  val of_string : string -> t option
  val of_string_exn : string -> t
  val to_string : t -> string
  val to_filename : t -> filename
  val compare : t -> t -> Ordering.t
  val equal : t -> t -> bool
  val hash : t -> int
  val to_dyn : t -> Dyn.t
  val drop_dot : t -> string

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t

  module Or_empty : sig
    type extension := t
    type t

    val check : t -> extension -> bool
    val of_string_exn : string -> t
    val to_string : t -> string
    val is_empty : t -> bool
    val is_extension : t -> bool
    val drop_suffix : string -> t -> string
    val extension : t -> extension option
    val extension_exn : t -> extension
  end
end

val extension : t -> Extension.Or_empty.t
val split_extension : t -> t * Extension.Or_empty.t
val split_extension_after_dot : t -> t * string
val add_extension : t -> Extension.t -> t
val set_extension : t -> Extension.t -> t
val extend : t -> suffix:t -> t
val actions_dir_basename : t
val bin_dir_basename : t
val cinaps_corrected : t
val cc_vendor : t
val checksum : t
val coqc : t
val corrected : t
val dev_tool_dir_basename : t
val dev_tool_locks_dir_basename : t
val doc_dir_basename : t
val doc_new_dir_basename : t
val dune : t
val dune_dir_basename : t
val dune_file : t
val dune_project : t
val dune_workspace : t
val expected : t
val fdo_profile : t
val formatted_dir_basename : t
val generated : t
val git_dir_basename : t
val gmake : t
val hg_dir_basename : t
val jbuild : t
val json : t
val js_dir_basename : t
val lock_dune : t
val linker_script : t
val lock_dir_basename : t
val make : t
val mdx_deps : t
val merlin_conf_dir_basename : t
val meta : t
val ocamlfind : t
val opam : t
val ppx_dir_basename : t
val pkg_dir_basename : t
val rocq : t
val run_t : t
val template : t
val topmod_dir_basename : t
val url : t
val utop_dir_basename : t
val findlib_conf : t

type program_name_kind =
  | In_path
  | Relative_to_current_dir
  | Absolute

val analyze_program_name : string -> program_name_kind
val equal : t -> t -> bool
val compare : t -> t -> Ordering.t
val hash : t -> int
val chop_extension : [ `Use_remove_extension ]

module Map : Map.S with type key = t
module Set : Set.S with type elt = t and type 'a map = 'a Map.t
module Array : Array_intf.S with type Set.elt = t
