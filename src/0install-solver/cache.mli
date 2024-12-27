module Make (CacheEntry : sig
    type t

    val compare : t -> t -> int
  end) : sig
  (** The cache is used in [build_problem], while the clauses are still being added. *)
  type 'a t

  module M : Map.S with type key = CacheEntry.t

  (** Once the problem is built, an immutable snapshot is taken. *)
  type 'a snapshot = 'a M.t

  val create : unit -> 'a t

  (** [lookup cache make key] will look up [key] in [cache].
      * If not found, create it with [value, process = make key], add [value] to the cache,
      * and then call [process ()] on it.
      * [make] must not be recursive (since the key hasn't been added yet),
      * but [process] can be. In other words, [make] does whatever setup *must*
      * be done before anyone can use this cache entry, while [process] does
      * setup that can be done afterwards. *)
  val lookup
    :  'a t
    -> (CacheEntry.t -> ('a * (unit -> unit Fiber.t)) Fiber.t)
    -> CacheEntry.t
    -> 'a Fiber.t

  val snapshot : 'a t -> 'a snapshot
  val get : CacheEntry.t -> 'a snapshot -> 'a option
  val get_exn : CacheEntry.t -> 'a snapshot -> 'a
  val filter_map : (CacheEntry.t -> 'a -> 'b option) -> 'a M.t -> 'b M.t
end
