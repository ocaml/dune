(** Merlin rules *)

type t =
  { requires   : (unit, Lib.t list) Build.t
  ; flags      : (unit, string list) Build.t
  ; preprocess : Jbuild.Preprocess.t
  ; libname    : string option
  }

val merge_two : t -> t -> t

val merge_all : t list -> t option

(** Add rules for generating the .merlin in a directory and return
    the final .merlin used *)
val add_rules : Super_context.t -> dir:Path.t -> t -> unit

