(** Library variants *)

(** Library variants allow to select the implementation of a library at link
    time.

    They are directly mapped to findlib predicates. *)

(* XXX technically this is all findlib/dune conventions, but we'll allow it for
   now *)

open Stdune

type t = private string

include Comparable_intf.S with type key := t

val make : string -> t
val to_string : t -> string
val encode : t Dune_sexp.Encoder.t
val decode : t Dune_sexp.Decoder.t

(** {1 Well-known variants} *)

val ppx_driver : t
val mt : t
val mt_posix : t
val byte : t
val native : t
val plugin : t
