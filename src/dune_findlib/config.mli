open Import

type t

val to_dyn : t -> Dyn.t

(** Finds the library search paths for this configuration, prepending
    [OCAMLPATH] if set *)
val path : t -> Path.t list

(** Finds program [prog] for this configuration, if it exists *)
val tool : t -> prog:string -> Path.t option Memo.t

val ocamlpath_sep : char

(** Read and parse the [OCAMLPATH] environment variable *)
val ocamlpath : Env.t -> Path.t list option

(** Interpret [env] findlib predicates in findlib configuration files. *)
val env : t -> Env.t

(** Load the findlib configuration for this [findlib_toolchain], if any. *)
val discover_from_env
  :  env:Env.t
  -> which:(Filename.t -> Path.t option Memo.t)
  -> ocamlpath:Path.t list
  -> findlib_toolchain:string option
  -> t option Memo.t
