(** Selected opaque packages in a workspace, indexed per build context.
    Reading outputs crosses the executor's cookie boundary. *)

open Import

type t =
  { context : Context_name.t
  ; dir : Path.Build.t
  ; stanza : Opam_stanza.t
  ; paths : Path.Build.t Opam_build.Paths.t
  }

val all : Context_name.t -> t Package.Name.Map.t Memo.t
val find : Context_name.t -> Package.Name.t -> t option Memo.t
val key : t -> string
val dependency : t -> Opam_build.Dependency.t Action_builder.t

(** Opaque providers from both workspace declarations and the lock directory. *)
val opaque_names : Context_name.t -> Package.Name.Set.t Memo.t

val find_opaque
  :  Context_name.t
  -> Package.Name.t
  -> (Path.Build.t * Opam_build.Dependency.t Action_builder.t) option Memo.t
