(** VCS handling *)

open Stdune

module Kind : sig
  type t =
    | Git
    | Hg

  val of_dir_contents : String.Set.t -> t option

  val of_filename : string -> t option
end

type t =
  { root : Path.t
  ; kind : Kind.t
  }

val equal : t -> t -> bool

val to_dyn : t -> Dyn.t

(** Nice description of the current tip *)
val describe : t -> string Fiber.t

(** String uniquely identifying the current head commit *)
val commit_id : t -> string Fiber.t

(** List of files committed in the repo *)
val files : t -> Path.t list Fiber.t

(** VCS commands *)
val git : Path.t Lazy.t

val hg : Path.t Lazy.t
