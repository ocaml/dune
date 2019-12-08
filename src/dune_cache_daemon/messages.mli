open Stdune

include module type of Messages_intf

val incoming_message_of_sexp :
  version -> Sexp.t -> (incoming message, string) Result.t

val initial_message_of_sexp : Sexp.t -> (initial message, string) Result.t

val outgoing_message_of_sexp :
  version -> Sexp.t -> (outgoing message, string) Result.t

val sexp_of_message : version -> 'a message -> Sexp.t
