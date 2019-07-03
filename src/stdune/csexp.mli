type t = Sexp.t

exception Parse_error of string

val to_buffer_canonical : buf:Buffer.t -> t -> unit
(** [to_buffer_canonical ~buf sexp] outputs the S-expression [sexp] converted
    to its canonical form to buffer [buf]. *)

val to_string_canonical : t -> string
(** [to_string_mach sexp] converts S-expression [sexp] to a string in
    canonical form. *)

val parse_canonical : char Stream.t -> t
(** [parse_canonical stream] reads one S-expression in canonical form
    from [stream] *)
