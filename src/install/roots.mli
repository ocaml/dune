open Import

type 'a t =
  { lib_root : 'a
  ; libexec_root : 'a
  ; bin : 'a
  ; sbin : 'a
  ; share_root : 'a
  ; etc_root : 'a
  ; doc_root : 'a
  ; man : 'a
  }

val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t

(** Compute the opam layout from prefix. the opam layout is used for _build *)
val opam_from_prefix : 'a -> relative:('a -> string -> 'a) -> 'a t

(** Some roots (e.g. libexec) have another roots as default (e.g. lib) *)
val complete : 'a option t -> 'a option t

val map : f:('a -> 'b) -> 'a t -> 'b t
val map2 : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

(** return the roots of the first argument if present *)
val first_has_priority : 'a option t -> 'a option t -> 'a option t

val to_env_without_path : 'a t -> relative:('a -> string -> 'a) -> (Env.Var.t * 'a) list
val add_to_env : Path.Build.t t -> Env.t -> Env.t
val make : 'a -> relative:('a -> string -> 'a) -> 'a t
val make_all : 'a -> 'a t
