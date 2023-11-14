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
  ; to_cwd : string list (** How to reach the cwd from the root *)
  ; reach_from_root_prefix : string
  (** Prefix filenames with this to reach them from the root *)
  ; kind : Kind.t
  }

val create : default_is_cwd:bool -> specified_by_user:string option -> t
