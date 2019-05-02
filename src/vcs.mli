(** VCS handling *)

open Stdune

module Kind : sig
  type t = Git | Hg

  val of_dir_contents : String.Set.t -> t option
end

type t =
  { root : Path.t
  ; kind : Kind.t
  }

val to_dyn : t -> Dyn.t

(** Output of [git describe --dirty --always] or hg equivalent *)
val describe : t -> string Fiber.t

(** VCS commands *)
val git : Path.t Lazy.t
val hg : Path.t Lazy.t
