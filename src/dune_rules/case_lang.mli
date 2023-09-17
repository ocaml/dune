(** The case language expresses conditional evaluation. The general form is:
    [(case %{expr} (<pred> -> <outcome>))] where [<pred>] is described using the
    predicate language: [Predicate_lang.t]. *)

open Import

type 'a t

val eval : 'a t -> f:(String_with_vars.t -> string) -> 'a option
val decode : 'a Dune_lang.Decoder.t -> 'a t Dune_lang.Decoder.t
