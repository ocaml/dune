(** Finding the root of the workspace *)

module Kind : sig
  type t =
    | Explicit
    | Dune_workspace
    | Dune_project
    | Cwd
end

type t =
  { dir : string
  ; to_cwd : string list  (** How to reach the cwd from the root *)
  ; kind : Kind.t  (** Closest VCS in directories strictly above the root *)
  ; ancestor_vcs : Dune_engine.Vcs.t option
  }

val create : specified_by_user:string option -> t
