(** Represent a path component.

    A path component is just a string without a '/' character. *)

include module type of struct
  include Stdlib.Filename
end

(* TODO add invariants and make this abstract or private *)
type t = string

module Extension : sig
  type t

  val ml : t
  val mli : t
  val mlpack : t
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
  val bc : t
  val bc_exe : t
  val ml_gen : t
  val cmt : t
  val cmti : t
  val cms : t
  val cmsi : t
  val odoc : t
  val h : t
  val d : t
  val all_deps : t
  val js : t
  val mlg : t
  val json : t
  val of_string : string -> t option
  val of_string_exn : string -> t
  val to_string : t -> string
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
    val extension : t -> extension option
    val extension_exn : t -> extension
  end
end

val extension : string -> Extension.Or_empty.t
val split_extension : t -> string * Extension.Or_empty.t
val split_extension_after_dot : t -> string * string

type program_name_kind =
  | In_path
  | Relative_to_current_dir
  | Absolute

val analyze_program_name : t -> program_name_kind
val equal : t -> t -> bool
val compare : t -> t -> Ordering.t
val chop_extension : [ `Use_remove_extension ]

module Set = String.Set
module Map = String.Map
