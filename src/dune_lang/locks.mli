(** Abstract locks. Use [Expander.expand_locks] to get paths suitable for
    [Action.Full.add_locks] or [Action.Full.make]. *)
type lock = private Lock of String_with_vars.t

type t = lock list

val field : ?check:unit Dune_sexp.Decoder.t -> unit -> t Dune_sexp.Decoder.fields_parser
