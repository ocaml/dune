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

(** Returns the path separator for a given env var, if it's a known
    path-like variable (e.g. OCAMLPATH uses [;] on all platforms). *)
val sep : Env.Var.t -> char option

(** The set of all path-like env vars managed by the install roots:
    PATH, OCAMLPATH, CAML_LD_LIBRARY_PATH, OCAMLFIND_IGNORE_DUPS_IN,
    OCAMLTOP_INCLUDE_PATH, and MANPATH. *)
val path_vars : Env.Var.Set.t

(** [extend_env_concat_path a b] extends [a] with the bindings from [b], with
    one twist: for every var in {!path_vars}, the value from [b] is prepended
    (cons-style, respecting per-var separator from {!sep}) to the value from
    [a] rather than overwriting it. Generalisation of
    [Env_path.extend_env_concat_path] to every install-managed path var. *)
val extend_env_concat_path : Env.t -> Env.t -> Env.t

val make : 'a -> relative:('a -> string -> 'a) -> 'a t
val make_all : 'a -> 'a t
