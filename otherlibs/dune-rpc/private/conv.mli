(** Bidirectional parsing of canonical s-expressions *)

open Import

type ('a, 'k) t

type values

type 'a value = ('a, values) t

val sexp : (Sexp.t, values) t

val int : (int, values) t

val unit : (unit, values) t

val char : (char, values) t

val string : (string, values) t

val list : ('a, values) t -> ('a list, values) t

val pair : ('a, values) t -> ('b, values) t -> ('a * 'b, values) t

val triple :
  ('a, values) t -> ('b, values) t -> ('c, values) t -> ('a * 'b * 'c, values) t

val enum : (string * 'a) list -> ('a, values) t

val iso : ('a, 'k) t -> ('a -> 'b) -> ('b -> 'a) -> ('b, 'k) t

val version : ?until:int * int -> ('a, 'k) t -> since:int * int -> ('a, 'k) t

(** {2 parsing records} *)

type fields

type 'a field

val required : ('a, values) t -> 'a field

val optional : ('a, values) t -> 'a option field

val field : string -> 'a field -> ('a, fields) t

val both : ('a, fields) t -> ('b, fields) t -> ('a * 'b, fields) t

val three :
  ('a, fields) t -> ('b, fields) t -> ('c, fields) t -> ('a * 'b * 'c, fields) t

val four :
     ('a, fields) t
  -> ('b, fields) t
  -> ('c, fields) t
  -> ('d, fields) t
  -> ('a * 'b * 'c * 'd, fields) t

val five :
     ('a, fields) t
  -> ('b, fields) t
  -> ('c, fields) t
  -> ('d, fields) t
  -> ('e, fields) t
  -> ('a * 'b * 'c * 'd * 'e, fields) t

val six :
     ('a, fields) t
  -> ('b, fields) t
  -> ('c, fields) t
  -> ('d, fields) t
  -> ('e, fields) t
  -> ('f, fields) t
  -> ('a * 'b * 'c * 'd * 'e * 'f, fields) t

val seven :
     ('a, fields) t
  -> ('b, fields) t
  -> ('c, fields) t
  -> ('d, fields) t
  -> ('e, fields) t
  -> ('f, fields) t
  -> ('g, fields) t
  -> ('a * 'b * 'c * 'd * 'e * 'f * 'g, fields) t

val eight :
     ('a, fields) t
  -> ('b, fields) t
  -> ('c, fields) t
  -> ('d, fields) t
  -> ('e, fields) t
  -> ('f, fields) t
  -> ('g, fields) t
  -> ('h, fields) t
  -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h, fields) t

val record : ('a, fields) t -> ('a, values) t

val either : ('a, fields) t -> ('b, fields) t -> (('a, 'b) Either.t, fields) t

(** For "untagged" sums, with no field name disambiguators.

    Note that, when decoding according to [either_untagged l r], [l] will be
    attempted first, then [r]. This means that cases in which the same sexp may
    be valid as both, a [Left] variant will be produced.

    You should avoid using this where possible. Its primary use case is when
    writing code that is generic over multiple different possible generations of
    a protocol, where the counterparty may be unaware of the existence of later
    versions.

    For example, consider multiple versions of a request being sent from two
    different clients:

    {v
    B (version 1) ->-.
                      \
                       A (server)
                      /
    C (version 2) ->-'
    v}

    To correctly decode the request, A must be able to parse both versions. If
    this happens before session version information is available (maybe as part
    of the version negotiation itself), the only way to do so is to attempt to *)
val either_untagged :
  ('a, values) t -> ('b, values) t -> (('a, 'b) Either.t, values) t

(** {2 parsing sums} *)

type ('a, 'arg) constr

val constr : string -> ('arg, values) t -> ('arg -> 'a) -> ('a, 'arg) constr

type case

val case : 'arg -> ('a, 'arg) constr -> case

type 'a econstr

val econstr : ('a, 'arg) constr -> 'a econstr

val sum : 'a econstr list -> ('a -> case) -> ('a, values) t

(** {2 conversion from/to} *)
type error =
  | Parse_error of
      { message : string
      ; payload : (string * Sexp.t) list
      }
  | Version_error of
      { since : int * int
      ; until : (int * int) option
      ; message : string
      ; payload : (string * Sexp.t) list
      }

val dyn_of_error : error -> Dyn.t

val to_sexp : ('a, values) t -> 'a -> Sexp.t

val of_sexp :
  ('a, values) t -> version:int * int -> Sexp.t -> ('a, error) result

val fdecl : ('a, 'k) t Fdecl.t -> ('a, 'k) t
