open Import

type t =
  | Atom of string
  | List of t list

exception Of_sexp_error of string * t

val of_sexp_error : string -> t -> _

module Locs : sig
  type t =
    | Atom of Loc.t
    | List of Loc.t * t list

  val loc : t -> Loc.t
  val sub_exn : t -> path:int list -> t
end

val to_string : t -> string

module To_sexp : sig
  type nonrec 'a t = 'a -> t
  val string     : string                    t
  val int        : int                       t
  val pair       : 'a t -> 'b t -> ('a * 'b) t
  val list       : 'a t -> 'a list           t
  val string_set : String_set.t              t
end

module Of_sexp : sig
  type nonrec 'a t = t -> 'a

  val string     : string                    t
  val int        : int                       t
  val pair       : 'a t -> 'b t -> ('a * 'b) t
  val list       : 'a t -> 'a list           t
  val string_set : String_set.t              t

  module Field_spec : sig
    type 'a t
  end

  module Fields_spec : sig
    type ('a, 'b) t =
      | [] : ('a, 'a) t
      | ( :: ) : 'a Field_spec.t * ('b, 'c) t -> ('a -> 'b, 'c) t
  end

  val field   : string -> ?default:'a -> 'a t -> 'a Field_spec.t
  val field_o : string -> 'a t -> 'a option Field_spec.t

  val record
    :  ('record_of_fields, 'record) Fields_spec.t
    -> 'record_of_fields
    -> 'record t
end
