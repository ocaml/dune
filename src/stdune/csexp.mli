type t = Sexp.t

exception Parse_error of string

val to_buffer : buf:Buffer.t -> t -> unit
(** [to_buffer ~buf sexp] outputs the S-expression [sexp] converted to
    its canonical form to buffer [buf]. *)

val to_string : t -> string
(** [to_string_mach sexp] converts S-expression [sexp] to a string in
    canonical form. *)

val parse : char Stream.t -> t
(** [parse stream] reads one S-expression in canonical form from
    [stream] *)
