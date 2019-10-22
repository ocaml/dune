open Stdune

include module type of Messages_intf

val message_of_sexp : Sexp.t -> (message, string) Result.t

val sexp_of_message : message -> Sexp.t
