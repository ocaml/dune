open Stdune

include module type of Messages_intf

val message_of_sexp : Sexp.t -> (message, string) Result.t
