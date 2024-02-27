(** Compilation contexts *)

(** Dune supports two different kind of contexts:

    - the default context, which correspond to the environment Dune is run, i.e.
      it takes [ocamlc] and other tools from the [PATH] and the ocamlfind
      configuration where it can find it

    - opam switch contexts, where one opam switch correspond to one context

    each context is built into a sub-directory of [Path.build_dir] (usually
    _build):

    - _build/default for the default context

    - _build/<switch> for other contexts

    Dune is able to build simultaneously against several contexts. In particular
    this allow for simple cross-compilation: when an executable running on the
    host is needed, it is obtained by looking in another context. *)

open Import

module Kind : sig
  type t =
    | Default
    | Opam of Opam_switch.t
    | Lock of { default : bool }
end

module Env_nodes : sig
  type t =
    { context : Dune_env.t option
    ; workspace : Dune_env.t option
    }
end

(** A context contains some basic things that are required to build just
    about anything. Note that accessing many fields is possible without
    initializing the context. This should be favored whenever this can break a
    potential dependency cycle. *)
type t

val ocaml : t -> Ocaml_toolchain.t Memo.t
val build_context : t -> Build_context.t
val kind : t -> Kind.t
val findlib_paths : t -> Path.t list Memo.t
val installed_env : t -> Env.t Memo.t
val default_ocamlpath : t -> Path.t list Memo.t
val findlib_toolchain : t -> Context_name.t option
val instrument_with : t -> Lib_name.t list
val profile : t -> Profile.t
val merlin : t -> bool
val equal : t -> t -> bool
val hash : t -> int
val to_dyn : t -> Dyn.t
val to_dyn_concise : t -> Dyn.t
val name : t -> Context_name.t
val which : t -> Filename.t -> Path.t option Memo.t

(** [Some path/to/foo.exe] if this contexts is for feedback-directed
    optimization of target path/to/foo.exe *)
val fdo_target_exe : t -> Path.t option

(** By default Dune builds and installs dynamically linked foreign
    archives (usually named [dll*.so]). It is possible to disable this by
    adding (disable_dynamically_linked_foreign_archives true) to the workspace
    file, in which case bytecode executables will be built with all foreign
    archives statically linked into the runtime system. *)
val dynamically_linked_foreign_archives : t -> bool Memo.t

(** [env_nodes] env nodes to initialize this context with. *)
val env_nodes : t -> Env_nodes.t

(** [path t] returns the [PATH] that this context uses to resolve binaries *)
val path : t -> Path.t list

(** If this context is a cross-compilation context, you need another
    context for building tools used for the compilation that run on the
    host. *)
val for_host : t -> t Memo.t option

(** Directory where artifact are stored, for instance "_build/default" *)
val build_dir : t -> Path.Build.t

(** [false] if a user explicitly listed this context in the workspace.
    Controls whether we add artifacts from this context \@install *)
val implicit : t -> bool

(** Compare the context names *)
val compare : t -> t -> Ordering.t

(** [map_exe t exe] returns a version of [exe] that is suitable for being
    executed on the current machine. For instance, if [t] is a cross-compilation
    build context, [map_exe t exe] returns the version of [exe] that lives in
    the host build context. Otherwise, it just returns [exe]. *)
val map_exe : t -> Path.t -> Path.t

(** Query where build artifacts should be installed if the user doesn't specify
    an explicit installation directory. *)
val roots : t -> Path.t option Install.Roots.t Memo.t

val host : t -> t Memo.t

module DB : sig
  val get : Context_name.t -> t Memo.t
  val all : unit -> t list Memo.t
  val by_dir : Path.Build.t -> t Memo.t
end
