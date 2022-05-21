(** Actions defined in dune files.

    All constructors correspond to actions the user may write in dune files.
    Eventually, these are all desugared into [Action.t], which are actions
    executed by the build system. *)

open Import
open Action_types

type t =
  | Run of String_with_vars.t * String_with_vars.t list
  | With_accepted_exit_codes of int Predicate_lang.t * t
  | Dynamic_run of String_with_vars.t * String_with_vars.t list
  | Chdir of String_with_vars.t * t
  | Setenv of String_with_vars.t * String_with_vars.t * t
  (* It's not possible to use a build String_with_vars.t here since jbuild
     supports redirecting to /dev/null. In [dune] files this is replaced with
     %{null} *)
  | Redirect_out of Outputs.t * String_with_vars.t * File_perm.t * t
  | Redirect_in of Inputs.t * String_with_vars.t * t
  | Ignore of Outputs.t * t
  | Progn of t list
  | Echo of String_with_vars.t list
  | Cat of String_with_vars.t
  | Copy of String_with_vars.t * String_with_vars.t
  | Symlink of String_with_vars.t * String_with_vars.t
  | Copy_and_add_line_directive of String_with_vars.t * String_with_vars.t
  | System of String_with_vars.t
  | Bash of String_with_vars.t
  | Write_file of String_with_vars.t * File_perm.t * String_with_vars.t
  | Mkdir of String_with_vars.t
  | Diff of (String_with_vars.t, String_with_vars.t) Diff.t
  | No_infer of t
  | Pipe of Outputs.t * t list
  | Cram of String_with_vars.t

include Dune_lang.Conv.S with type t := t

(** Raises User_error on invalid action. *)
val validate : loc:Loc.t -> t -> unit

val compare_no_locs : t -> t -> Ordering.t

val to_dyn : t -> Dyn.t

val remove_locs : t -> t

val equal : t -> t -> bool

val chdir : String_with_vars.t -> t -> t

val run : String_with_vars.t -> String_with_vars.t list -> t
