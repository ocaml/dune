(** Compilation contexts *)

(** jbuild supports two different kind of contexts:

    - the default context, which correspond to the environment jbuild is run, i.e. it
    takes [ocamlc] and other tools from the [PATH] and the ocamlfind configuration where
    it can find it

    - opam switch contexts, where one opam switch correspond to one context

    each context is built into a sub-directory of "_build":

    - _build/default for the default context
    - _build/<switch> for other contexts

    jbuild is able to build simultaneously against several contexts. In particular this
    allow for simple cross-compilation: when an executable running on the host is needed,
    it is obtained by looking in another context.
*)

open! Import

module Kind : sig
  type t = Default | Opam of { root : string; switch : string }
end

type t =
  { kind : Kind.t

  ; (** If this context is a cross-compilation context, you need another context for
        building tools used for the compilation that run on the host. *)
    for_host : t option

  ; (** Directory where artifact are stored, for instance "_build/default" *)
    build_dir : Path.t

  ; (** [PATH] *)
    path : Path.t list

  ; (** Ocaml bin directory with all ocaml tools *)
    ocaml_bin  : Path.t
  ; ocaml      : Path.t
  ; ocamlc     : Path.t
  ; ocamlopt   : Path.t option
  ; ocamldep   : Path.t
  ; ocamllex   : Path.t
  ; ocamlyacc  : Path.t
  ; ocamlmklib : Path.t

  ; (** Environment variables *)
    env : string array

  ; (** Where to look for META files *)
    findlib_path : Path.t list

  ; (** Misc *)
    arch_sixtyfour : bool

  ; opam_var_cache : (string, string) Hashtbl.t

  ; (** Output of [ocamlc -config] *)
    version                 : string
  ; stdlib_dir              : Path.t
  ; ccomp_type              : string
  ; bytecomp_c_compiler     : string
  ; bytecomp_c_libraries    : string
  ; native_c_compiler       : string
  ; native_c_libraries      : string
  ; native_pack_linker      : string
  ; ranlib                  : string
  ; cc_profile              : string
  ; architecture            : string
  ; system                  : string
  ; ext_obj                 : string
  ; ext_asm                 : string
  ; ext_lib                 : string
  ; ext_dll                 : string
  ; os_type                 : string
  ; default_executable_name : string
  ; host                    : string
  ; target                  : string
  ; flambda                 : bool
  ; exec_magic_number       : string
  ; cmi_magic_number        : string
  ; cmo_magic_number        : string
  ; cma_magic_number        : string
  ; cmx_magic_number        : string
  ; cmxa_magic_number       : string
  ; ast_impl_magic_number   : string
  ; ast_intf_magic_number   : string
  ; cmxs_magic_number       : string
  ; cmt_magic_number        : string
  }

val create_for_opam : ?root:string -> switch:string -> unit -> t Future.t

val default : t Future.t Lazy.t

(** All contexts in use, by name *)
val all : unit -> t String_map.t

val which : t -> string -> Path.t option

val extend_env : vars:string String_map.t -> env:string array -> string array

val opam_config_var : t -> string -> string option Future.t

val install_prefix : t -> Path.t Future.t
