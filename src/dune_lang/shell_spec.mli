(** {!type:t} holds settings directly present in [dune] files and no expansion
    of {!Exec_file_shell} is performed *)

open Dune_sexp

type t =
  | System_shell (** when selected, [env sh] is used; this is the {b default} *)
  | Custom_shell of
      { prog : String_with_vars.t
      ; args : String_with_vars.t list
      }
      (** when [Custom_shell] is selected, [prog, args] are expanded by
          {!module:Dune_rules.Action_unexpanded.Action_expander.E.prog_and_args}
          and are used as the shell program. *)

val default : t
val map : (String_with_vars.t -> String_with_vars.t) -> t -> t
val encode : t -> Dune_sexp.t list
val decode : t Decoder.t
