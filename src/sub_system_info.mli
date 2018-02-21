(** Raw sub-systems informations *)

type t = ..

(** Register a sub-system parser. If [short] is provided, then its
    value is used in case the sub-system is specified as just
    [(name)]. *)
val register
  :  name:Sub_system_name.t
  -> ?short:t
  -> of_sexp:t Sexp.Of_sexp.t
  -> unit
  -> unit

(** Parse some contents from a library stanza *)
val parse : unit -> t Sub_system_name.Map.t Sexp.Of_sexp.record_parser
