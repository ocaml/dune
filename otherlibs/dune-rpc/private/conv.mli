(** Bidirectional parsing of canonical s-expressions *)

open Import

type ('a, 'k) t
type values
type 'a value = ('a, values) t

val sexp : (Sexp.t, values) t
val int : (int, values) t
val float : (float, values) t
val unit : (unit, values) t
val char : (char, values) t
val string : (string, values) t
val list : ('a, values) t -> ('a list, values) t
val pair : ('a, values) t -> ('b, values) t -> ('a * 'b, values) t
val option : ('a, values) t -> ('a option, values) t

val triple
  :  ('a, values) t
  -> ('b, values) t
  -> ('c, values) t
  -> ('a * 'b * 'c, values) t

val enum : (string * 'a) list -> ('a, values) t

(** [iso t to_ from] creates a parser for a type ['b] out of a parser for a
    type ['a], where ['a] and ['b] are isomorphic to one another. The functions
    [to_] and [from] convert between the two types ['a] and ['b]. A typical
    approach for parsing record types is to convert them to/from tuples (via the
    [three], [four], etc. combinators) which can be parsed with [record], and
    then use [iso] to convert the parser for a tuple type into a parser for the
    original record type. *)
val iso : ('a, 'k) t -> ('a -> 'b) -> ('b -> 'a) -> ('b, 'k) t

val iso_result : ('a, 'k) t -> ('a -> ('b, exn) result) -> ('b -> 'a) -> ('b, 'k) t
val version : ?until:int * int -> ('a, 'k) t -> since:int * int -> ('a, 'k) t

(** {2 parsing records} *)

type fields
type 'a field

val required : ('a, values) t -> 'a field
val optional : ('a, values) t -> 'a option field
val field : string -> 'a field -> ('a, fields) t
val both : ('a, fields) t -> ('b, fields) t -> ('a * 'b, fields) t
val three : ('a, fields) t -> ('b, fields) t -> ('c, fields) t -> ('a * 'b * 'c, fields) t

val four
  :  ('a, fields) t
  -> ('b, fields) t
  -> ('c, fields) t
  -> ('d, fields) t
  -> ('a * 'b * 'c * 'd, fields) t

val five
  :  ('a, fields) t
  -> ('b, fields) t
  -> ('c, fields) t
  -> ('d, fields) t
  -> ('e, fields) t
  -> ('a * 'b * 'c * 'd * 'e, fields) t

val six
  :  ('a, fields) t
  -> ('b, fields) t
  -> ('c, fields) t
  -> ('d, fields) t
  -> ('e, fields) t
  -> ('f, fields) t
  -> ('a * 'b * 'c * 'd * 'e * 'f, fields) t

val seven
  :  ('a, fields) t
  -> ('b, fields) t
  -> ('c, fields) t
  -> ('d, fields) t
  -> ('e, fields) t
  -> ('f, fields) t
  -> ('g, fields) t
  -> ('a * 'b * 'c * 'd * 'e * 'f * 'g, fields) t

val eight
  :  ('a, fields) t
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

val error : error -> 'a
val dyn_of_error : error -> Dyn.t
val to_sexp : ('a, values) t -> 'a -> Sexp.t
val of_sexp : ('a, values) t -> version:int * int -> Sexp.t -> ('a, error) result

(** [fixpoint f] is a helper for creating parsers of recursive data structures
    such as ASTs. [f] is a function which returns a parser for a single node in
    the hierarchy, and [f] is passed a parser which it can use for parsing
    children of the current node. [fixpoint f] then returns a parser for the
    recursive data structure. *)
val fixpoint : (('a, 'k) t -> ('a, 'k) t) -> ('a, 'k) t

val sexp_for_digest : ('a, 'k) t -> Sexp.t
