open! Stdune

val cmi : Lib.t -> Arg_spec.glob
val cmi_and_cmx : Lib.t -> Arg_spec.glob

module L : sig
  (** [file_deps t libs ~ext] returns a list of path dependencies for all the
      files with extension [ext] of libraries [libs]. *)
  val file_deps
    :  Lib.L.t
    -> exts:string list
    -> Arg_spec.glob list

  val file_deps_with_exts
    : (Lib.t * string list) list
    -> Arg_spec.glob list

  val headers : Lib.t list -> Arg_spec.glob list

  val cmi         : Lib.L.t -> Arg_spec.glob list
  val cmi_and_cmx : Lib.L.t -> Arg_spec.glob list
end
