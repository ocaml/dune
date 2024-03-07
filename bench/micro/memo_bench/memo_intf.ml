module type Monad_intf = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val map : 'a t -> f:('a -> 'b) -> 'b t

  module Let_syntax : sig
    val return : 'a -> 'a t
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  end
end

module type Test_env = sig
  module Glass : sig
    type t

    val create : unit -> t
    val break : t -> unit
  end

  module Io : sig
    include Monad_intf

    module Ivar : sig
      type 'a io := 'a t
      type 'a t

      val create : unit -> 'a t
      val read : 'a t -> 'a io
      val fill : 'a t -> 'a -> unit io
    end

    val of_thunk : (unit -> 'a t) -> 'a t
  end

  module Memo : sig
    include Monad_intf

    val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
    val all : 'a t list -> 'a list t
    val of_glass : Glass.t -> 'a -> 'a t
    val of_thunk : (unit -> 'a t) -> 'a t
    val of_io : (unit -> 'a Io.t) -> 'a t
    val memoize : 'a t -> 'a t
  end

  module Var : sig
    type 'a t

    val create : 'a -> 'a t
    val set : 'a t -> 'a -> unit
    val read : 'a t -> 'a Memo.t

    (** peek once without registering interest in future updates *)
    val peek : 'a t -> 'a
  end

  val run : 'a Memo.t -> 'a
  val make_counter : unit -> int Memo.t * (unit -> unit)
end
