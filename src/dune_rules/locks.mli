open! Import

(** Abstract locks. Use [Expander.expand_locks] to get paths suitable for
    [Action.Full.add_locks] or [Action.Full.make]. *)
type lock = private Lock of String_with_vars.t

type t = lock list

val field :
  ?check:unit Dune_lang.Decoder.t -> unit -> t Dune_lang.Decoder.fields_parser
