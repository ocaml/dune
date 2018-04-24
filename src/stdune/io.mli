(** IO operations *)

val open_in  : ?binary:bool (* default true *) -> Path.t -> in_channel
val open_out : ?binary:bool (* default true *) -> Path.t -> out_channel

val close_in  : in_channel  -> unit
val close_out : out_channel -> unit

val with_file_in  : ?binary:bool (* default true *) -> Path.t -> f:(in_channel -> 'a) -> 'a
val with_file_out : ?binary:bool (* default true *) -> Path.t -> f:(out_channel -> 'a) -> 'a

val with_lexbuf_from_file : Path.t -> f:(Lexing.lexbuf -> 'a) -> 'a

val lines_of_file : Path.t -> string list

val read_file : Path.t -> string
val write_file : Path.t -> string -> unit

val compare_files : Path.t -> Path.t -> Ordering.t

val write_lines : Path.t -> string list -> unit

val copy_channels : in_channel -> out_channel -> unit

val copy_file : src:Path.t -> dst:Path.t -> unit

val read_all : in_channel -> string

module Sexp : sig
  val load : Path.t -> mode:'a Sexp.Parser.Mode.t -> 'a
  val load_many_as_one : Path.t -> Sexp.Ast.t
end

(**/**)
(* used in jbuild_load *)
val buf_len : int
