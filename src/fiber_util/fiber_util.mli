(** Utilities for working with the Fiber library. *)

open! Stdune

(** [Temp.Monad] instantiated to the Fiber monad. *)
module Temp : sig
  val with_temp_file
    :  dir:Path.t
    -> prefix:string
    -> suffix:string
    -> f:(Path.t Or_exn.t -> 'a Fiber.t)
    -> 'a Fiber.t

  val with_temp_dir
    :  parent_dir:Path.t
    -> prefix:string
    -> suffix:string
    -> f:(Path.t Or_exn.t -> 'a Fiber.t)
    -> 'a Fiber.t
end
