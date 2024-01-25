open Import

type t =
  | Data_only
  | Normal
  | Vendored

val to_string : t -> string
val to_dyn : t -> Dyn.t

module Or_ignored : sig
  type nonrec t =
    | Ignored
    | Status of t
end

module Map : sig
  type status := t

  type 'a t =
    { data_only : 'a
    ; vendored : 'a
    ; normal : 'a
    }

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val init : f:(status -> 'a) -> 'a t
  val merge : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val find : 'a t -> status -> 'a
  val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t
end

module Set : sig
  type status := t
  type t = bool Map.t

  val all : t
  val normal_only : t
  val to_list : t -> status list
end

module Per_dir : sig
  type t

  val status : t -> dir:Filename.t -> Or_ignored.t
end

module Spec : sig
  type t
  type input = (Loc.t * Predicate_lang.Glob.t) option Map.t

  val default : t
  val create : input -> t
  val eval : t -> dirs:Filename.t list -> Per_dir.t
end
