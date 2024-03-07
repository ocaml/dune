(** Toolchain information derived from an OCaml installation *)

open Import

type t =
  { bin_dir : Path.t
  ; ocaml : Action.Prog.t
  ; ocamlc : Path.t
  ; ocamlopt : Action.Prog.t
  ; ocamldep : Action.Prog.t
  ; ocamlmklib : Action.Prog.t
  ; ocamlobjinfo : Action.Prog.t
  ; ocaml_config : Ocaml_config.t
  ; ocaml_config_vars : Ocaml_config.Vars.t
  ; version : Ocaml.Version.t
  ; builtins : Meta.Simplified.t Package.Name.Map.t Memo.t
  ; lib_config : Lib_config.t
  }

val of_env_with_findlib
  :  Context_name.t
  -> Env.t
  -> Findlib_config.t option
  -> which:(Filename.t -> Path.t option Memo.t)
  -> t Memo.t

val of_binaries : path:Path.t list -> Context_name.t -> Env.t -> Path.Set.t -> t Memo.t

(** Return the compiler needed for this compilation mode *)
val compiler : t -> Ocaml.Mode.t -> Action.Prog.t

(** The best compilation mode for this context *)
val best_mode : t -> Mode.t

val check_fdo_support : t -> Context_name.t -> unit
val register_response_file_support : t -> unit
