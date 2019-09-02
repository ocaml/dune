type t = Sexp.t

(** [to_string sexp] converts S-expression [sexp] to a string in canonical
    form. *)
val to_string : t -> string

(** [parse_string string] parses [string] into S-expression *)
val parse_string : string -> (t, string) Result.t

(** [to_buffer ~buf sexp] outputs the S-expression [sexp] converted to its
    canonical form to buffer [buf]. *)
val to_buffer : buf:Buffer.t -> t -> unit

(** [parse stream] reads one S-expression in canonical form from [stream] *)
val parse : char Stream.t -> (t, string) Result.t
