open Stdune

include module type of Messages_intf

(** Decode an [incoming] message. *)
val incoming_message_of_sexp :
  version -> Sexp.t -> (incoming message, string) Result.t

(** Decode an [initial] message. *)
val initial_message_of_sexp : Sexp.t -> (initial message, string) Result.t

(** Decode an [outgoing] message. *)
val outgoing_message_of_sexp :
  version -> Sexp.t -> (outgoing message, string) Result.t

(** Encode a message. *)
val sexp_of_message : version -> 'a message -> Sexp.t

(** Send a message. *)
val send : version -> out_channel -> 'a message -> unit

(** Find the newest [version] of the communication protocol supported both by
    Dune and the cache daemon. To do that, we send [versions_supported_by_dune]
    to the cache daemon via the [out_channel], receive the supported versions of
    the cache daemon via the [in_channel], and pick the newest one that matches
    both lists. *)
val negotiate_version :
     versions_supported_by_dune:version list
  -> Unix.file_descr
  -> in_channel
  -> out_channel
  -> (version, string) result

val string_of_version : version -> string

val hint_supported : version -> bool

val hint_min_version : version

val find_newest_common_version :
  version list -> version list -> (version, string) result
