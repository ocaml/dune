module type Combinators = sig
  type 'a t
  val unit       : unit                      t
  val string     : string                    t
  val int        : int                       t
  val float      : float                     t
  val bool       : bool                      t
  val pair       : 'a t -> 'b t -> ('a * 'b) t
  val triple     : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val list       : 'a t -> 'a list           t
  val array      : 'a t -> 'a array          t
  val option     : 'a t -> 'a option         t
  val string_set : String.Set.t              t
  val string_map : 'a t -> 'a String.Map.t   t
  val string_hashtbl : 'a t -> (string, 'a) Hashtbl.t t
end
