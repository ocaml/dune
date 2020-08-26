(** Representation of (diff ...) actions *)

open Stdune

module Mode : sig
  type t =
    | Binary  (** no diffing, just raw comparison *)
    | Text  (** diffing after newline normalization *)
end

type ('path, 'target) t =
  { optional : bool
  ; mode : Mode.t
  ; file1 : 'path
  ; file2 : 'target
  }

val decode :
     'path Dune_lang.Decoder.t
  -> 'target Dune_lang.Decoder.t
  -> optional:bool
  -> ('path, 'target) t Dune_lang.Decoder.t

val decode_binary :
     'path Dune_lang.Decoder.t
  -> 'target Dune_lang.Decoder.t
  -> ('path, 'target) t Dune_lang.Decoder.t

val eq_files : (Path.t, Path.Build.t) t -> bool
