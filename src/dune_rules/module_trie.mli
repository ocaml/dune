open Import

type 'a t = 'a node Module_name.Map.t

and 'a node =
  | Leaf of 'a
  | Map of 'a t

type key = Module_name.Path.t

val empty : 'a t

val map : 'a t -> f:('a -> 'b) -> 'b t

val mapi : 'a t -> f:(key -> 'a -> 'b) -> 'b t

val of_map : 'a Module_name.Map.t -> 'a t

val find : 'a t -> key -> 'a option

val set : 'a t -> key -> 'a -> 'a t

val set_map : 'a t -> key -> 'a Module_name.Map.t -> 'a t

val remove : 'a t -> key -> 'a t

val mem : 'a t -> key -> bool

val is_empty : _ t -> bool

val fold : 'a t -> init:'acc -> f:('a -> 'acc -> 'acc) -> 'acc

val to_list_map : 'a t -> f:(key -> 'a -> 'b) -> 'b list

val foldi : 'a t -> init:'acc -> f:(key -> 'a -> 'acc -> 'acc) -> 'acc

val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t

val to_map : 'a t -> 'a Module_name.Map.t

val values : 'a t -> 'a list

val exists : 'a t -> f:('a -> bool) -> bool

val singleton : key -> 'a -> 'a t

val merge :
  'a t -> 'b t -> f:(key -> 'a option -> 'b option -> 'c option) -> 'c t

val as_singleton : 'a t -> 'a option

val filter_map : 'a t -> f:('a -> 'b option) -> 'b t

val decode : src_dir:Path.t -> Module.t t Dune_lang.Decoder.t

val encode : Module.t t -> Dune_lang.t list

module Memo_traversals : sig
  val parallel_map : 'a t -> f:(key -> 'a -> 'b Memo.t) -> 'b t Memo.t
end

val toplevel_only : 'a t -> 'a Module_name.Map.t
