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
  }

val of_env_with_findlib :
     Context_name.t
  -> Env.t
  -> Findlib.Config.t option
  -> which:(Filename.t -> Path.t option Memo.t)
  -> t Memo.t

(** Return the compiler needed for this compilation mode *)
val compiler : t -> Ocaml.Mode.t -> Action.Prog.t

(** The best compilation mode for this context *)
val best_mode : t -> Mode.t
