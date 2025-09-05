(** OCaml flags *)

open Import

type t

val make
  :  spec:Dune_lang.Link_flags.Spec.t
  -> default:t
  -> eval:
       (Ordered_set_lang.Unexpanded.t
        -> standard:string list Action_builder.t
        -> string list Action_builder.t)
  -> t

val default : default_cxx_link_flags:string list Action_builder.t -> t
val get : use_standard_cxx_flags:bool -> t -> string list Action_builder.t
val dump : t -> Dune_lang.t list Action_builder.t
