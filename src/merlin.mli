(** Merlin rules *)

type t =
  { requires   : (unit, Lib.t list) Build.t
  ; flags      : string list
  ; preprocess : Jbuild.Preprocess.t
  ; libname    : string option
  }

(** Add rules for generating the .merlin in a directory *)
val add_rules : Super_context.t -> dir:Path.t -> t list -> unit

