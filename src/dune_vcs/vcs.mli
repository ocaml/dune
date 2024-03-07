(** VCS handling *)

open Stdune

module Kind : sig
  type t =
    | Git
    | Hg

  val of_dir_name : Filename.t -> t option
  val of_dir_contents : Filename.Set.t -> t option
end

type t =
  { root : Path.t
  ; kind : Kind.t
  }

val equal : t -> t -> bool
val to_dyn : t -> Dyn.t

(** Nice description of the current tip *)
val describe : t -> string option Memo.t

(** String uniquely identifying the current head commit *)
val commit_id : t -> string option Memo.t

(** List of files committed in the repo *)
val files : t -> Path.Source.t list Memo.t

(** VCS commands *)
val git : Path.t Lazy.t

val hg : Path.t Lazy.t

(** Valid git exit codes *)
val git_accept : unit -> ('a, ('a, int) result) Dune_engine.Process.Failure_mode.t
