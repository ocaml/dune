(** Library variants *)

(** Library variants allow to select the implementation of a library at link
    time.

    They are directly mapped to findlib predicates. *)

include Stdune.Interned_intf.S

(** Well-known variants *)
val ppx_driver : t

val mt : t

val mt_posix : t

val byte : t

val native : t

val plugin : t

val encode : t Dune_lang.Encoder.t

val decode : t Dune_lang.Decoder.t
