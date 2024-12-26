module Make (CacheEntry : sig
    type t
    type value

    val compare : t -> t -> int
  end) : sig
  (** The cache is used in [build_problem], while the clauses are still being added. *)
  type t

  module M : Map.S with type key = CacheEntry.t

  (** Once the problem is built, an immutable snapshot is taken. *)
  type snapshot = CacheEntry.value M.t

  val create : unit -> t

  (** [lookup cache make key] will look up [key] in [cache].
      * If not found, create it with [value, process = make key], add [value] to the cache,
      * and then call [process ()] on it.
      * [make] must not be recursive (since the key hasn't been added yet),
      * but [process] can be. In other words, [make] does whatever setup *must*
      * be done before anyone can use this cache entry, while [process] does
      * setup that can be done afterwards. *)
  val lookup
    :  t
    -> (CacheEntry.t -> (CacheEntry.value * (unit -> unit Fiber.t)) Fiber.t)
    -> CacheEntry.t
    -> CacheEntry.value Fiber.t

  val snapshot : t -> snapshot
  val get : CacheEntry.t -> snapshot -> CacheEntry.value option
  val get_exn : CacheEntry.t -> snapshot -> CacheEntry.value
  val filter_map : (CacheEntry.t -> 'a -> 'b option) -> 'a M.t -> 'b M.t
end
