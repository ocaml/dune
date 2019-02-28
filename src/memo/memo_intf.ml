open Stdune

module type Sexpable = sig
  type t
  val to_sexp : t -> Sexp.t
end

module type Data = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
  val to_sexp : t -> Sexp.t
end

module type Input = Data

module type Decoder = sig
  type t
  val decode : t Dune_lang.Decoder.t
end

module type S = sig
  type input

  (** Type of memoized functions *)
  type 'a t

  (** [create name ?allow_cutoff ouput_spec f] creates a memoized version
      of [f]. The result of [f] for a given input is cached, so that
      the second time [exec t x] is called, the previous result is
      re-used if possible.

      [exec t x] tracks what calls to other memoized functions [f x]
      performs. When the result of such dependent call changes, [exec t
      x] will automatically recompute [f x].

      Running the computation may raise [Memo.Cycle_error.E] if a cycle is
      detected.  *)
  val create
    :  string
    -> ?allow_cutoff:bool
    -> doc:string
    -> (module Data with type t = 'output)
    -> (input -> 'output Fiber.t)
    -> 'output t


  (** Same as [create] except that the implementation is defined at a
      latter point of the program. This is useful for creating mutually
      recursive memoized functions.  *)
  val fcreate
    :  string
    -> ?allow_cutoff:bool
    -> doc:string
    -> (module Data with type t = 'output)
    -> 'output t

  (** Execute a memoized function *)
  val exec : 'a t -> input -> 'a Fiber.t
end

module type S_sync = sig
  type input

  (** Type of memoized functions *)
  type 'a t

  (** [create name ?allow_cutoff ouput_spec f] creates a memoized version
      of [f]. The result of [f] for a given input is cached, so that
      the second time [exec t x] is called, the previous result is
      re-used if possible.

      [exec t x] tracks what calls to other memoized functions [f x]
      performs. When the result of such dependent call changes, [exec t
      x] will automatically recompute [f x].

      Running the computation may raise [Memo.Cycle_error.E] if a cycle is
      detected.  *)
  val create
    :  string
    -> ?allow_cutoff:bool
    -> doc:string
    -> (module Data with type t = 'output)
    -> (input -> 'output)
    -> 'output t

  val fcreate
    :  string
    -> ?allow_cutoff:bool
    -> doc:string
    -> (module Data with type t = 'output)
    -> 'output t

  (** Execute a memoized function *)
  val exec : 'a t -> input -> 'a
end
