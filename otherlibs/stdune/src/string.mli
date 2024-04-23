type t = string

include module type of struct
    include StringLabels
  end
  with type t := t

val equal : t -> t -> bool
val compare : t -> t -> Ordering.t
val hash : t -> int
val to_dyn : t -> Dyn.t
val break : t -> pos:int -> t * t
val is_empty : t -> bool
val of_list : char list -> t
val is_prefix : t -> prefix:t -> bool
val is_suffix : t -> suffix:t -> bool
val take : t -> int -> t
val drop : t -> int -> t
val split_n : t -> int -> t * t
val drop_prefix : t -> prefix:t -> t option
val drop_prefix_if_exists : t -> prefix:t -> t
val drop_suffix : t -> suffix:t -> t option
val drop_suffix_if_exists : t -> suffix:t -> t

(** [drop_prefix_and_suffix t ~prefix ~suffix] Will attempt to remove [prefix]
    from the prefix and [suffix] from the suffix of [t]. Return [Some _] only
    if the [suffix] and [prefix] were present. *)
val drop_prefix_and_suffix : t -> prefix:t -> suffix:t -> t option

module Caseless : sig
  (** Case-insensitive matching semantics. *)

  val drop_prefix : t -> prefix:t -> t option
  val drop_prefix_if_exists : t -> prefix:t -> t
  val drop_suffix : t -> suffix:t -> t option
  val drop_suffix_if_exists : t -> suffix:t -> t
end

(** These only change ASCII characters *)
val capitalize : t -> t

val uncapitalize : t -> t
val uppercase : t -> t
val lowercase : t -> t
val index : t -> char -> int option
val index_from : t -> int -> char -> int option
val rindex : t -> char -> int option
val rindex_from : t -> int -> char -> int option
val extract_words : t -> is_word_char:(char -> bool) -> t list
val extract_comma_space_separated_words : t -> t list
val extract_blank_separated_words : t -> t list
val lsplit2 : t -> on:char -> (t * t) option
val lsplit2_exn : t -> on:char -> t * t
val rsplit2 : t -> on:char -> (t * t) option

(** [split t ~on] returns the list of non-overlapping substrings of
    [t] between each occurence of [on]. If [t] begins or ends with [on]
    then an empty string will be present on the "far" side of [on] in the
    output. If [on] does not appear in [t] then the result is a list
    containing [t] (even if [t] is the empty string).

    Note that [split "" ~on] returns [[""]] (ie. a list containing a single
    empty string).

    This function is roughly the inverse of [concat].
    Ie. [concat ~sep:(String.make 1 c) (split ~on:c s)] will return
    the original string [s]. *)
val split : t -> on:char -> t list

val split_lines : t -> t list

(** Escape ONLY one character. {!escape} also escapes '\n',... and transforms
    all chars above '~' into '\xxx' which is not suitable for UTF-8 strings. *)
val escape_only : char -> t -> t

(** Return the length of the longest string in the list *)
val longest : string list -> int

val longest_map : 'a list -> f:('a -> string) -> int
val longest_prefix : t list -> t
val exists : t -> f:(char -> bool) -> bool
val for_all : t -> f:(char -> bool) -> bool

(** [maybe_quoted s] is [s] if [s] doesn't need escaping according to OCaml
    lexing conventions and [sprintf "%S" s] otherwise.

    (* CR-someday aalekseyev: this function is not great: barely anything "needs
    escaping according to OCaml lexing conventions", so the condition for
    whether to add the quote characters ends up being quite arbitrary. *) *)
val maybe_quoted : t -> t

val quoted : t -> t

(** Produces: "x, y and z" *)
val enumerate_and : string list -> string

(** Produces: "x, y or z" *)
val enumerate_or : string list -> string

(** Produces: "One of x, y or z" *)
val enumerate_one_of : t list -> t

(** Find index of first character satisfying [f] *)
val findi : string -> f:(char -> bool) -> int option

(** Find index of last character satisfying [f] *)
val rfindi : string -> f:(char -> bool) -> int option

include Comparable_intf.S with type key := t
module Table : Hashtbl.S with type key = t

(** Whether the string needs quoting if it is part of a shell command *)
val need_quoting : string -> bool

(** [quote_for_shell s] quotes [s] using [Filename.quote] if [need_quoting s] is
    [true] *)
val quote_for_shell : string -> string

(** [quote_list_for_shell l] is
    [List.map l ~f:quote_for_shell |> concat ~sep:" "] *)
val quote_list_for_shell : string list -> string

val filter_map : string -> f:(char -> char option) -> string
val contains_double_underscore : string -> bool
