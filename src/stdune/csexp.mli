type t = Sexp.t

module type Stream = sig
  type input

  type t

  val make : input -> t

  val peek_byte : t -> int

  val input_byte : t -> int

  val input_string : t -> int -> string
end

module StringStream : Stream with type input = string

module ChannelStream : Stream with type input = in_channel

module Parser (S : Stream) : sig
  val parse : S.input -> t

  val parse_stream : S.t -> t
end

val to_buffer_canonical : buf:Buffer.t -> t -> unit
(** [to_buffer_canonical ~buf sexp] outputs the S-expression [sexp] converted
      to its canonical form to buffer [buf]. *)

val to_string_canonical : t -> string
(** [to_string_mach sexp] converts S-expression [sexp] to a string in
    canonical form. *)

val parse_canonical : string -> t

val parse_channel_canonical : in_channel -> t
