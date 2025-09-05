(** OCaml flags *)

open! Import

type t

val make
  :  spec:Dune_lang.Ocaml_flags.Spec.t
  -> default:t
  -> eval:
       (Ordered_set_lang.Unexpanded.t
        -> standard:string list Action_builder.t
        -> string list Action_builder.t)
  -> t

val allow_only_melange : t -> t
val default : dune_version:Dune_lang.Syntax.Version.t -> profile:Profile.t -> t
val empty : t
val of_list : string list -> t
val get : t -> Lib_mode.t -> string list Action_builder.t
val append_common : t -> string list -> t
val with_vendored_alerts : t -> t
val dump : t -> Dune_lang.t list Action_builder.t
val with_vendored_flags : t -> ocaml_version:Version.t -> t
val open_flags : Module_name.t list -> string list
