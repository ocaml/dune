(** Internal, unreleased entrance to the opaque package executor. *)

open Import

type t =
  { loc : Loc.t
  ; package : Package.t
  ; dir : Filename.t
  ; version : Package_version.t
  ; dev : bool
  ; source : String_with_vars.t option
  ; files : (String_with_vars.t * Path.Local.t) list
  ; recipe : Opam_build.Recipe.t
  }

include Stanza.S with type t := t

val decode : t Dune_lang.Decoder.t
