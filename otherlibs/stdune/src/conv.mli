(** Bidirectional parsing of canonical s-expressions *)

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
    [to_] and [from] convert between the two types ['a] and ['b]. For record
    types, [Record] avoids the need to convert to and from tuples. *)
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

(** Record construction for decoding, paired with field projections for encoding.
    The constructor and projections must agree, as with the two directions of
    [iso]. For example:
    {[
      record
        (Record.make (fun name age -> { name; age })
         |> Record.field "name" (required string) ~get:(fun t -> t.name)
         |> Record.field "age" (optional int) ~get:(fun t -> t.age)
         |> Record.finish)
    ]} *)
module Record : sig
  type ('record, 'remaining) builder

  val make : 'constructor -> ('record, 'constructor) builder

  (** Consume one constructor argument, using [get] to recover it when encoding.
      Fields are decoded in the order they are added. Field names must not
      overlap with previously added fields. *)
  val add
    :  ('a, fields) t
    -> get:('record -> 'a)
    -> ('record, 'a -> 'remaining) builder
    -> ('record, 'remaining) builder

  (** [field name spec] is [add (field name spec)]. *)
  val field
    :  string
    -> 'a field
    -> get:('record -> 'a)
    -> ('record, 'a -> 'remaining) builder
    -> ('record, 'remaining) builder

  (** Finish once all constructor arguments have been supplied. The resulting
      fields may be composed with other fields, or wrapped with [record]. *)
  val finish : ('record, 'record) builder -> ('record, fields) t
end

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
