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
end

val locate         : t      -> sub:t -> locs:Locs.t      -> Loc.t option
val locate_in_list : t list -> sub:t -> locs:Locs.t list -> Loc.t option

val to_string : t -> string

module type Combinators = sig
  type 'a t
  val unit       : unit                      t
  val string     : string                    t
  val int        : int                       t
  val bool       : bool                      t
  val pair       : 'a t -> 'b t -> ('a * 'b) t
  val list       : 'a t -> 'a list           t
  val option     : 'a t -> 'a option         t
  val string_set : String_set.t              t
  val string_map : 'a t -> 'a String_map.t   t
end

module To_sexp : Combinators with type 'a t = 'a -> t

module Of_sexp : sig
  include Combinators with type 'a t = t -> 'a

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
  val field_b : string -> bool Field_spec.t

  val record
    :  ?ignore:string list
    -> ('record_of_fields, 'record) Fields_spec.t
    -> 'record_of_fields -> 'record t

  module Constructor_spec : sig
    type 'a t
  end

  module Constructor_args_spec : sig
    type 'a conv = 'a t
    type ('a, 'b) t =
      | [] : ('a, 'a) t
      | ( :: ) : 'a conv * ('b, 'c) t -> ('a -> 'b, 'c) t
  end with type 'a conv := 'a t

  val cstr : string -> ('a, 'b) Constructor_args_spec.t -> 'a -> 'b Constructor_spec.t

  val sum
    :  'a Constructor_spec.t list
    -> 'a t
end
