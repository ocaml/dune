open Stdune

include module type of Messages_intf

val incoming_message_of_sexp :
  version -> Sexp.t -> (incoming message, string) Result.t

val initial_message_of_sexp : Sexp.t -> (initial message, string) Result.t

val outgoing_message_of_sexp :
  version -> Sexp.t -> (outgoing message, string) Result.t

val sexp_of_message : version -> 'a message -> Sexp.t

val send : version -> out_channel -> 'a message -> unit

val negotiate_version :
     version list
  -> Unix.file_descr
  -> char Stream.t
  -> out_channel
  -> (version, string) result

val string_of_version : version -> string
