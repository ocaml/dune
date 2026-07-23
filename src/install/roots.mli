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

(** Cons every install root onto its corresponding env var: the [lib] dirs onto
    [OCAMLPATH], [share] onto [OCAMLFIND_IGNORE_DUPS_IN], [bin] onto [PATH],
    etc. The generic "make an opam-style install prefix discoverable via the
    standard path env vars" primitive. Used both by [dune exec]'s staging cons
    (see [bin/exec.ml]) and by the per-package-set scoped layout (see
    [Install_layout.env]). *)
val add_to_env : Path.Build.t t -> Env.t -> Env.t

(** Returns the path separator for a given env var, if it's a known
    path-like variable (e.g. OCAMLPATH uses [;] on all platforms). *)
val sep : Env.Var.t -> char option

(** Cons a build path onto a path-like env var, using the per-var separator
    from {!sep}. *)
val cons_path : Env.t -> var:Env.Var.t -> Path.Build.t -> Env.t

(** The set of all path-like env vars managed by the install roots:
    PATH, OCAMLPATH, CAML_LD_LIBRARY_PATH, OCAMLFIND_IGNORE_DUPS_IN,
    OCAMLTOP_INCLUDE_PATH, and MANPATH. *)
val path_vars : Env.Var.Set.t

(** [extend_env_concat_path_vars a b] extends [a] with the bindings from [b],
    with one twist: for every var in {!path_vars}, the value from [b] is
    prepended (cons-style, respecting per-var separator from {!sep}) to the
    value from [a] rather than overwriting it. Generalisation of
    [Env_path.extend_env_concat_path] (which only handles {!Env_path.var}) to
    every install-managed path var. *)
val extend_env_concat_path_vars : Env.t -> Env.t -> Env.t

val make : 'a -> relative:('a -> string -> 'a) -> 'a t
val make_all : 'a -> 'a t
