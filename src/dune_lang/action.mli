(** Actions defined in dune files.

    All constructors correspond to actions the user may write in dune files.
    Eventually, these are all desugared into [Action.t], which are actions
    executed by the build system. *)

open Stdune
open Dune_sexp

module Action_plugin : sig
  val syntax : Syntax.t
end

module Diff : sig
  module Mode : sig
    type t =
      | Binary  (** no diffing, just raw comparison *)
      | Text  (** diffing after newline normalization *)
  end

  type ('path, 'target) t =
    { optional : bool
    ; mode : Mode.t
    ; file1 : 'path
    ; file2 : 'target
    }

  val map : ('p, 't) t -> path:('p -> 'x) -> target:('t -> 'y) -> ('x, 'y) t

  val decode :
       'path Decoder.t
    -> 'target Decoder.t
    -> optional:bool
    -> ('path, 'target) t Decoder.t

  val decode_binary :
    'path Decoder.t -> 'target Decoder.t -> ('path, 'target) t Decoder.t
end

module Outputs : sig
  type t =
    | Stdout
    | Stderr
    | Outputs  (** Both Stdout and Stderr *)

  val to_string : t -> string
end

module Inputs : sig
  type t = Stdin

  val to_string : t -> string
end

module File_perm : sig
  (** File mode, for when creating files. We only allow what Dune takes into
      account when memoizing commands. *)

  type t =
    | Normal
    | Executable

  val suffix : t -> string

  val to_unix_perm : t -> int
end

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
  | Cat of String_with_vars.t list
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

include Conv.S with type t := t

(** Raises User_error on invalid action. *)
val validate : loc:Loc.t -> t -> unit

val compare_no_locs : t -> t -> Ordering.t

val to_dyn : t -> Dyn.t

val remove_locs : t -> t

val equal : t -> t -> bool

val chdir : String_with_vars.t -> t -> t

val run : String_with_vars.t -> String_with_vars.t list -> t
