open Import

type t

val to_dyn : t -> Dyn.t
val ocamlpath_var : Env.Var.t
val ocamlpath_sep : char
val ocamlfind_ignore_dups_in : Env.Var.t

(** Finds the library search paths for this configuration, prepending
    [OCAMLPATH] if set *)
val ocamlpath : t -> Path.t list Memo.t

(** Finds program [prog] for this configuration, if it exists *)
val tool : t -> prog:string -> Path.t option Memo.t

(** Read and parse the [OCAMLPATH] environment variable *)
val ocamlpath_of_env : Env.t -> Path.t list option

(** Interpret [env] findlib predicates in findlib configuration files. *)
val env : t -> Env.t

(** Load the findlib configuration for this [findlib_toolchain], if any. *)
val discover_from_env
  :  env:Env.t
  -> which:(Filename.t -> Path.t option Memo.t)
  -> ocamlpath:Path.t list Memo.t
  -> findlib_toolchain:string option
  -> t option Memo.t
