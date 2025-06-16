(** Patch - parsing and applying unified diffs in pure OCaml *)

type hunk = {
  mine_start : int ;
  mine_len : int ;
  mine : string list ;
  their_start : int ;
  their_len : int ;
  their : string list ;
}
(** A hunk contains some difference between two files: each with a start line
    and length, and then the content as lists of string. *)

type parse_error = {
  msg : string;
  lines : string list;
}

exception Parse_error of parse_error

val pp_hunk : mine_no_nl:bool -> their_no_nl:bool -> Format.formatter -> hunk -> unit
(** [pp_hunk ppf hunk] pretty-prints the [hunk] on [ppf], the printing is in the
    same format as [diff] does. *)

type git_ext =
  | Rename_only of string * string
  | Delete_only
  | Create_only

type operation =
  | Edit of string * string
  | Delete of string
  | Create of string
  | Git_ext of (string * string * git_ext)
  (** The operation of a diff: in-place [Edit], [Delete], [Create].
      And its git-extensions: [Rename_only], [Delete_only], [Create_only].
      The parameters to the variants are filenames.

      Note that [Edit] also renames the given file under certain conditions
      and the file to use is driven by this POSIX rule:
      https://pubs.opengroup.org/onlinepubs/9799919799/utilities/patch.html#tag_20_92_13_02

      Note also that the two filenames in [Git_ext] represent what would be
      in [git --diff <filename1> <filename2>] with their respective prefixes
      removed if parsed with [parse ~p:1] or above.

      Warning: The two parameters of [Rename_only] represent the values of the
      [rename from <filename1>] and [rename to <filename2>] following the
      specs of the git extensions. Following the behaviour of GNU Patch which
      ignores these two lines, it is recommended to get the filenames from
      [Git_ext] instead of from [Rename_only], which are used only for
      pretty-printing. *)

val pp_operation : Format.formatter -> operation -> unit
(** [pp_operation ppf op] pretty-prints the operation [op] on [ppf]. *)

val operation_eq : operation -> operation -> bool
(** [operation_eq a b] is true if [a] and [b] are equal. *)

type t = {
  operation : operation ;
  hunks : hunk list ;
  mine_no_nl : bool ;
  their_no_nl : bool ;
}
(** The type of a diff: an operation, a list of hunks, and information whether
    a trailing newline exists on the left and right. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] pretty-prints [t] on [ppf]. *)

val pp_list : Format.formatter -> t list -> unit
(** [pp ppf diffs] pretty-prints [diffs] on [ppf]. *)

val parse : p:int -> string -> t list
(** [parse ~p data] decodes [data] as a list of diffs.

    @param p denotes the expected prefix level of the filenames.
    For more information, see the option [-p] in your POSIX-complient
    patch.

    @raise Parse_error if a filename was unable to be parsed *)

val patch : cleanly:bool -> string option -> t -> string option
(** [patch file_contents diff] applies [diff] on [file_contents], resulting in
    the new file contents (or None if deleted). *)

val diff : (string * string) option -> (string * string) option -> t option
(** [diff (filename_a, content_a) (filename_b, content_b)] creates a diff between
    [content_a] and [content_b]. Returns [None] if no changes could be detected.

    @raise Invalid_argument if both arguments are [None]. *)
