(** These functions will either build or watch their arguments before accessing
    them *)

open Stdune

val dir_contents : Path.t -> (Filename.t list, Unix_error.Detailed.t) result Memo.t
val file_exists : Path.t -> bool Memo.t
val dir_exists : Path.t -> bool Memo.t
val with_lexbuf_from_file : Path.t -> f:(Lexing.lexbuf -> 'a) -> 'a Memo.t
