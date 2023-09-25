(** Actions defined in dune files.

    All constructors correspond to actions the user may write in dune files.
    Eventually, these are all desugared into [Action.t], which are actions
    executed by the build system. *)

open Stdune
open Dune_sexp
open Dune_util.Action

module Action_plugin : sig
  val syntax : Syntax.t
end

module Diff : sig
  open Diff

  module Mode : sig
    type t = Mode.t =
      | Binary
      | Text
  end

  type nonrec ('path, 'target) t = ('path, 'target) t =
    { optional : bool
    ; mode : Mode.t
    ; file1 : 'path
    ; file2 : 'target
    }

  val decode
    :  'path Decoder.t
    -> 'target Decoder.t
    -> optional:bool
    -> ('path, 'target) t Decoder.t

  val decode_binary : 'path Decoder.t -> 'target Decoder.t -> ('path, 'target) t Decoder.t
end

module Outputs : sig
  type t = Outputs.t =
    | Stdout
    | Stderr
    | Outputs (** Both Stdout and Stderr *)

  val to_string : t -> string
end

module Inputs : sig
  type t = Inputs.t = Stdin

  val to_string : t -> string
end

module File_perm : sig
  (** File mode, for when creating files. We only allow what Dune takes into
      account when memoizing commands. *)

  type t = File_perm.t =
    | Normal
    | Executable

  val suffix : t -> string
  val to_unix_perm : t -> int
end

module Env_update : sig
  module Op : sig
    type t =
      | Eq
      | PlusEq
      | EqPlus
      | ColonEq
      | EqColon
      | EqPlusEq
  end

  type 'a t =
    { op : Op.t
    ; var : Env.Var.t
    ; value : 'a
    }

  val map : 'a t -> f:('a -> 'b) -> 'b t
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val to_dyn : 'a Dyn.builder -> 'a t Dyn.builder
  val decode : String_with_vars.t t Decoder.t
  val encode : String_with_vars.t t -> Dune_sexp.t
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
  | Concurrent of t list
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
  | Cram of String_with_vars.t * Shell_spec.t
  | Patch of String_with_vars.t
  | Substitute of String_with_vars.t * String_with_vars.t
  | Withenv of String_with_vars.t Env_update.t list * t
  | When of Blang.t * t

val encode : t Encoder.t
val decode_dune_file : t Decoder.t
val decode_pkg : t Decoder.t

(** Raises User_error on invalid action. *)
val validate : loc:Loc.t -> t -> unit

val compare_no_locs : t -> t -> Ordering.t
val equal_no_locs : t -> t -> bool
val to_dyn : t -> Dyn.t
val remove_locs : t -> t
val equal : t -> t -> bool
val chdir : String_with_vars.t -> t -> t
val run : String_with_vars.t -> String_with_vars.t list -> t
