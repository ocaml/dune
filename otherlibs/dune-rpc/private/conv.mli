(** Bidirectional parsing of canonical s-expressions *)

open Import

type ('a, 'k) t

type values

type 'a value = ('a, values) t

val sexp : (Sexp.t, values) t

val int : (int, values) t

val unit : (unit, values) t

val string : (string, values) t

val list : ('a, values) t -> ('a list, values) t

val pair : ('a, values) t -> ('b, values) t -> ('a * 'b, values) t

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

val dyn_of_error : error -> Dyn.t

val to_sexp : ('a, values) t -> 'a -> Sexp.t

val of_sexp :
  ('a, values) t -> version:int * int -> Sexp.t -> ('a, error) result
