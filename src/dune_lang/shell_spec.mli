(** {!type:t} holds settings directly present in [dune] files and no expansion
    of {!Exec_file_shell} is performed *)

open Dune_sexp

type t =
  | System_shell
      (** when selected, {v env sh v} is used; this is the {b default} *)
  | Bash_shell  (** when selected, {v env bash v} is used *)
  | Exec_file_shell of String_with_vars.t
      (** when [Exec_file_shell p] is selected, [p] is expanded by
          {!module:Dune_rules.Expander} as the path to a single executable,
          which is then used as the shell program *)

val default : t

val map : (String_with_vars.t -> String_with_vars.t) -> t -> t

val encode : t Encoder.t

val decode : t Decoder.t
