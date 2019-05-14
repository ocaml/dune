(** Representation of (diff ...) actions *)

module Mode : sig
  type t =
    | Binary      (** no diffing, just raw comparison      *)
    | Text        (** diffing after newline normalization  *)
    | Text_jbuild (** diffing but no newline normalization *)
end

type 'path t =
  { optional : bool
  ; mode     : Mode.t
  ; file1    : 'path
  ; file2    : 'path
  }

val decode
  :  'path Dune_lang.Decoder.t
  -> optional:bool
  -> 'path t Dune_lang.Decoder.t

val decode_binary
  :  'path Dune_lang.Decoder.t
  -> 'path t Dune_lang.Decoder.t
