(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

(** Module [Re]: code for creating and using regular expressions,
    independently of regular expression syntax. *)

(** Regular expression *)
type t = Ast.t

(** Compiled regular expression *)
type re = Compile.re

(** Manipulate matching groups. *)
module Group : sig
  (** Information about groups in a match. As is conventional, every
      match implicitly has a group 0 that covers the whole match, and
      explicit groups are numbered from 1. *)
  type t = Group.t

  (** Raise [Not_found] if the group did not match *)
  val get : t -> int -> string

  (** Similar to {!get}, but returns an option instead of using an exception. *)
  val get_opt : t -> int -> string option

  (** Raise [Not_found] if the group did not match *)
  val offset : t -> int -> int * int

  (** Similar to {!offset}, but returns an option instead of using an exception. *)
  val offset_opt : t -> int -> (int * int) option

  (** Return the start of the match. Raise [Not_found] if the group did not match. *)
  val start : t -> int -> int

  (** Similar to {!start_opt}, but returns an option instead of using an exception. *)
  val start_opt : t -> int -> int option

  (** Return the end of the match. Raise [Not_found] if the group did not match. *)
  val stop : t -> int -> int

  (** Similar to {!stop_opt}, but returns an option instead of using an exception. *)
  val stop_opt : t -> int -> int option

  (** Return the empty string for each group which did not match *)
  val all : t -> string array

  (** Return [(-1,-1)] for each group which did not match *)
  val all_offset : t -> (int * int) array

  (** Test whether a group matched *)
  val test : t -> int -> bool

  (** Returns the total number of groups defined - matched or not.
      This function is experimental. *)
  val nb_groups : t -> int

  val pp : Format.formatter -> t -> unit
end

type groups = Group.t [@@ocaml.deprecated "Use Group.t"]

(** {2 Compilation and execution of a regular expression} *)

(** Compile a regular expression into an executable version that can be
    used to match strings, e.g. with {!exec}. *)
val compile : t -> re

(** Return the number of capture groups (including the one
    corresponding to the entire regexp). *)
val group_count : re -> int

(** Return named capture groups with their index. *)
val group_names : re -> (string * int) list

(** [exec re str] searches [str] for a match of the compiled expression [re],
    and returns the matched groups if any.

    More specifically, when a match exists, [exec] returns a match that
    starts at the earliest position possible. If multiple such matches are
    possible, the one specified by the match semantics described below is
    returned.

    {5 Examples:}
    {[
      # let regex = Re.compile Re.(seq [str "//"; rep print ]);;
      val regex : re = <abstr>

      # Re.exec regex "// a C comment";;
      - : Re.Group.t = <abstr>

      # Re.exec regex "# a C comment?";;
      Exception: Not_found

      # Re.exec ~pos:1 regex "// a C comment";;
      Exception: Not_found
    ]}

    @param pos optional beginning of the string (default 0)
    @param len
      length of the substring of [str] that can be matched (default [-1],
      meaning to the end of the string)
    @raise Not_found if the regular expression can't be found in [str] *)
val exec
  :  ?pos:int (** Default: 0 *)
  -> ?len:int (** Default: -1 (until end of string) *)
  -> re
  -> string
  -> Group.t

(** Similar to {!exec}, but returns an option instead of using an exception.

    {5 Examples:}
    {[
      # let regex = Re.compile Re.(seq [str "//"; rep print ]);;
      val regex : re = <abstr>

      # Re.exec_opt regex "// a C comment";;
      - : Re.Group.t option = Some <abstr>

      # Re.exec_opt regex "# a C comment?";;
      - : Re.Group.t option = None

      # Re.exec_opt ~pos:1 regex "// a C comment";;
      - : Re.Group.t option = None
    ]} *)
val exec_opt
  :  ?pos:int (** Default: 0 *)
  -> ?len:int (** Default: -1 (until end of string) *)
  -> re
  -> string
  -> Group.t option

(** Similar to {!exec}, but returns [true] if the expression matches,
    and [false] if it doesn't. This function is more efficient than
    calling {!exec} or {!exec_opt} and ignoring the returned group.

    {5 Examples:}
    {[
      # let regex = Re.compile Re.(seq [str "//"; rep print ]);;
      val regex : re = <abstr>

      # Re.execp regex "// a C comment";;
      - : bool = true

      # Re.execp ~pos:1 regex "// a C comment";;
      - : bool = false
    ]} *)
val execp
  :  ?pos:int (** Default: 0 *)
  -> ?len:int (** Default: -1 (until end of string) *)
  -> re
  -> string
  -> bool

(** More detailed version of {!execp}. [`Full] is equivalent to [true],
    while [`Mismatch] and [`Partial] are equivalent to [false], but [`Partial]
    indicates the input string could be extended to create a match.

    {5 Examples:}
    {[
      # let regex = Re.compile Re.(seq [bos; str "// a C comment"]);;
      val regex : re = <abstr>

      # Re.exec_partial regex "// a C comment here.";;
      - : [ `Full | `Mismatch | `Partial ] = `Full

      # Re.exec_partial regex "// a C comment";;
      - : [ `Full | `Mismatch | `Partial ] = `Partial

      # Re.exec_partial regex "//";;
      - : [ `Full | `Mismatch | `Partial ] = `Partial

      # Re.exec_partial regex "# a C comment?";;
      - : [ `Full | `Mismatch | `Partial ] = `Mismatch
    ]} *)
val exec_partial
  :  ?pos:int (** Default: 0 *)
  -> ?len:int (** Default: -1 (until end of string) *)
  -> re
  -> string
  -> [ `Full | `Partial | `Mismatch ]

(** More detailed version of {!exec_opt}. [`Full group] is equivalent to [Some group],
    while [`Mismatch] and [`Partial _] are equivalent to [None], but [`Partial position]
    indicates that the input string could be extended to create a match, and no match could
    start in the input string before the given position.
    This could be used to not have to search the entirety of the input if more
    becomes available, and use the given position as the [?pos] argument. *)
val exec_partial_detailed
  :  ?pos:int (** Default: 0 *)
  -> ?len:int (** Default: -1 (until end of string) *)
  -> re
  -> string
  -> [ `Full of Group.t | `Partial of int | `Mismatch ]

(** Marks *)
module Mark : sig
  (** Mark id *)
  type t

  (** Tell if a mark was matched. *)
  val test : Group.t -> t -> bool

  module Set : Set.S with type elt = t

  (** Return all the mark matched. *)
  val all : Group.t -> Set.t

  val equal : t -> t -> bool
  val compare : t -> t -> int
end

(** {2 High Level Operations} *)

type split_token =
  [ `Text of string (** Text between delimiters *)
  | `Delim of Group.t (** Delimiter *)
  ]

(** Repeatedly calls {!exec} on the given string, starting at given position and
    length.

    {5 Examples:}
    {[
      # let regex = Re.compile Re.(seq [str "my"; blank; word(rep alpha)]);;
      val regex : re = <abstr>

      # Re.all regex "my head, my shoulders, my knees, my toes ...";;
      - : Re.Group.t list = [<abstr>; <abstr>; <abstr>; <abstr>]

      # Re.all regex "My head, My shoulders, My knees, My toes ...";;
      - : Re.Group.t list = []
    ]} *)
val all : ?pos:int -> ?len:int -> re -> string -> Group.t list

type 'a gen = unit -> 'a option

(** @deprecated Use {!module-Seq.all} instead. *)
val all_gen : ?pos:int -> ?len:int -> re -> string -> Group.t gen
[@@ocaml.deprecated "Use Seq.all"]

(** @deprecated Use {!module-Seq.all} instead. *)
val all_seq : ?pos:int -> ?len:int -> re -> string -> Group.t Seq.t
[@@ocaml.deprecated "Use Seq.all"]

(** Same as {!all}, but extracts the matched substring rather than returning
    the whole group. This basically iterates over matched strings.

    {5 Examples:}
    {[
      # let regex = Re.compile Re.(seq [str "my"; blank; word(rep alpha)]);;
      val regex : re = <abstr>

      # Re.matches regex "my head, my shoulders, my knees, my toes ...";;
      - : string list = ["my head"; "my shoulders"; "my knees"; "my toes"]

      # Re.matches regex "My head, My shoulders, My knees, My toes ...";;
      - : string list = []

      # Re.matches regex "my my my my head my 1 toe my ...";;
      - : string list = ["my my"; "my my"]

      # Re.matches ~pos:2 regex "my my my my head my +1 toe my ...";;
      - : string list = ["my my"; "my head"]
    ]} *)
val matches : ?pos:int -> ?len:int -> re -> string -> string list

(** @deprecated Use {!module-Seq.matches} instead. *)
val matches_gen : ?pos:int -> ?len:int -> re -> string -> string gen
[@@ocaml.deprecated "Use Seq.matches"]

(** @deprecated Use {!module-Seq.matches} instead. *)
val matches_seq : ?pos:int -> ?len:int -> re -> string -> string Seq.t
[@@ocaml.deprecated "Use Seq.matches"]

(** [split re s] splits [s] into chunks separated by [re]. It yields
    the chunks themselves, not the separator. An occurence of the
    separator at the beginning or the end of the string is ignoring.

    {5 Examples:}
    {[
      # let regex = Re.compile (Re.char ',');;
      val regex : re = <abstr>

      # Re.split regex "Re,Ocaml,Jerome Vouillon";;
      - : string list = ["Re"; "Ocaml"; "Jerome Vouillon"]

      # Re.split regex "No commas in this sentence.";;
      - : string list = ["No commas in this sentence."]

      # Re.split regex ",1,2,";;
      - : string list = ["1"; "2"]

      # Re.split ~pos:3 regex "1,2,3,4. Commas go brrr.";;
      - : string list = ["3"; "4. Commas go brrr."]
    ]}

    {6 Zero-length patterns:}

    Be careful when using [split] with zero-length patterns like [eol], [bow],
    and [eow].  Because they don't have any width, they will still be present in
    the result. (Note the position of the [\n] and space characters in the
    output.)

    {[
      # Re.split (Re.compile Re.eol) "a\nb";;
      - : string list = ["a"; "\nb"]

      # Re.split (Re.compile Re.bow) "a b";;
      - : string list = ["a "; "b"]

      # Re.split (Re.compile Re.eow) "a b";;
      - : string list = ["a"; " b"]
    ]}

    Compare this to the behavior of splitting on the char itself. (Note that
    the delimiters are not present in the output.)

    {[
      # Re.split (Re.compile (Re.char '\n')) "a\nb";;
      - : string list = ["a"; "b"]

      # Re.split (Re.compile (Re.char ' ')) "a b";;
      - : string list = ["a"; "b"]
    ]} *)
val split : ?pos:int -> ?len:int -> re -> string -> string list

(** [split_delim re s] splits [s] into chunks separated by [re]. It
    yields the chunks themselves, not the separator. Occurences of the
    separator at the beginning or the end of the string will produce
    empty chunks.

    {5 Examples:}
    {[
      # let regex = Re.compile (Re.char ',');;
      val regex : re = <abstr>

      # Re.split regex "Re,Ocaml,Jerome Vouillon";;
      - : string list = ["Re"; "Ocaml"; "Jerome Vouillon"]

      # Re.split regex "No commas in this sentence.";;
      - : string list = ["No commas in this sentence."]

      # Re.split regex ",1,2,";;
      - : string list = [""; "1"; "2"; ""]

      # Re.split ~pos:3 regex "1,2,3,4. Commas go brrr.";;
      - : string list = ["3"; "4. Commas go brrr."]
    ]}

    {6 Zero-length patterns:}

    Be careful when using [split_delim] with zero-length patterns like [eol],
    [bow], and [eow].  Because they don't have any width, they will still be
    present in the result. (Note the position of the [\n] and space characters
    in the output.)

    {[
      # Re.split_delim (Re.compile Re.eol) "a\nb";;
      - : string list = ["a"; "\nb"; ""]

      # Re.split_delim (Re.compile Re.bow) "a b";;
      - : string list = [""; "a "; "b"]

      # Re.split_delim (Re.compile Re.eow) "a b";;
      - : string list = ["a"; " b"; ""]
    ]}

    Compare this to the behavior of splitting on the char itself. (Note that
    the delimiters are not present in the output.)

    {[
      # Re.split_delim (Re.compile (Re.char '\n')) "a\nb";;
      - : string list = ["a"; "b"]

      # Re.split_delim (Re.compile (Re.char ' ')) "a b";;
      - : string list = ["a"; "b"]
    ]} *)
val split_delim : ?pos:int -> ?len:int -> re -> string -> string list

(** @deprecated Use {!module-Seq.split} instead. *)
val split_gen : ?pos:int -> ?len:int -> re -> string -> string gen
[@@ocaml.deprecated "Use Seq.split"]

(** @deprecated Use {!module-Seq.split} instead. *)
val split_seq : ?pos:int -> ?len:int -> re -> string -> string Seq.t
[@@ocaml.deprecated "Use Seq.split"]

(** [split re s] splits [s] into chunks separated by [re]. It yields the chunks
    along with the separators. For instance this can be used with a
    whitespace-matching re such as ["[\t ]+"].

    {5 Examples:}
    {[
      # let regex = Re.compile (Re.char ',');;
      val regex : re = <abstr>

      # Re.split_full regex "Re,Ocaml,Jerome Vouillon";;
      - : Re.split_token list =
        [`Text "Re"; `Delim <abstr>; `Text "Ocaml"; `Delim <abstr>;
        `Text "Jerome Vouillon"]

      # Re.split_full regex "No commas in this sentence.";;
      - : Re.split_token list = [`Text "No commas in this sentence."]

      # Re.split_full ~pos:3 regex "1,2,3,4. Commas go brrr.";;
      - : Re.split_token list =
        [`Delim <abstr>; `Text "3"; `Delim <abstr>; `Text "4. Commas go brrr."]
    ]} *)
val split_full : ?pos:int -> ?len:int -> re -> string -> split_token list

(** @deprecated Use {!module-Seq.split_full} instead. *)
val split_full_gen : ?pos:int -> ?len:int -> re -> string -> split_token gen
[@@ocaml.deprecated "Use Seq.split_full"]

(** @deprecated Use {!module-Seq.split_full} instead. *)
val split_full_seq : ?pos:int -> ?len:int -> re -> string -> split_token Seq.t
[@@ocaml.deprecated "Use Seq.split_full"]

module Seq : sig
  (** Same as {!module-Re.val-all} but returns an iterator.

      {5 Examples:}
      {[
        # let regex = Re.compile Re.(seq [str "my"; blank; word(rep alpha)]);;
        val regex : re = <abstr>

        # Re.Seq.all regex "my head, my shoulders, my knees, my toes ...";;
        - : Re.Group.t Seq.t = <fun>
      ]}
      @since 1.10.0 *)
  val all : ?pos:int (** Default: 0 *) -> ?len:int -> re -> string -> Group.t Seq.t

  (** Same as {!module-Re.val-matches}, but returns an iterator.

      {5 Example:}
      {[
        # let regex = Re.compile Re.(seq [str "my"; blank; word(rep alpha)]);;
        val regex : re = <abstr>

        # Re.Seq.matches regex "my head, my shoulders, my knees, my toes ...";;
        - : string Seq.t = <fun>
      ]}
      @since 1.10.0 *)
  val matches : ?pos:int (** Default: 0 *) -> ?len:int -> re -> string -> string Seq.t

  (** Same as {!module-Re.val-split} but returns an iterator.

      {5 Example:}
      {[
        # let regex = Re.compile (Re.char ',');;
        val regex : re = <abstr>

        # Re.Seq.split regex "Re,Ocaml,Jerome Vouillon";;
        - : string Seq.t = <fun>
      ]}
      @since 1.10.0 *)
  val split : ?pos:int (** Default: 0 *) -> ?len:int -> re -> string -> string Seq.t

  (** Same as {!module-Re.val-split_delim} but returns an iterator.

      {5 Example:}
      {[
        # let regex = Re.compile (Re.char ',');;
        val regex : re = <abstr>

        # Re.Seq.split regex "Re,Ocaml,Jerome Vouillon";;
        - : string Seq.t = <fun>
      ]}
      @since 1.11.1 *)
  val split_delim : ?pos:int (** Default: 0 *) -> ?len:int -> re -> string -> string Seq.t

  (** Same as {!module-Re.val-split_full} but returns an iterator.

      {5 Example:}
      {[
        # let regex = Re.compile (Re.char ',');;
        val regex : re = <abstr>

        # Re.Seq.split_full regex "Re,Ocaml,Jerome Vouillon";;
        - : Re.split_token Seq.t = <fun>
      ]}
      @since 1.10.0 *)
  val split_full
    :  ?pos:int (** Default: 0 *)
    -> ?len:int
    -> re
    -> string
    -> split_token Seq.t
end

(** {2 String expressions (literal match)} *)

val str : string -> t
val char : char -> t

(** {2 Basic operations on regular expressions} *)

(** Alternative.

    [alt []] is equivalent to {!empty}.

    By default, the leftmost match is preferred (see match semantics below). *)
val alt : t list -> t

(** Sequence *)
val seq : t list -> t

(** Match nothing *)
val empty : t

(** Empty word *)
val epsilon : t

(** 0 or more matches *)
val rep : t -> t

(** 1 or more matches *)
val rep1 : t -> t

(** [repn re i j] matches [re] at least [i] times
    and at most [j] times, bounds included.
    [j = None] means no upper bound. *)
val repn : t -> int -> int option -> t

(** 0 or 1 matches *)
val opt : t -> t

(** {2 String, line, word}

    We define a word as a sequence of latin1 letters, digits and underscore. *)

(** Beginning of line *)
val bol : t

(** End of line *)
val eol : t

(** Beginning of word *)
val bow : t

(** End of word *)
val eow : t

(** Beginning of string. This differs from {!start} because it matches
    the beginning of the input string even when using [~pos] arguments:

    {[
      let b = execp (compile (seq [ bos; str "a" ])) "aa" ~pos:1 in
      assert (not b)
    ]} *)
val bos : t

(** End of string. This is different from {!stop} in the way described
    in {!bos}. *)
val eos : t

(** Last end of line or end of string *)
val leol : t

(** Initial position. This differs from {!bos} because it takes into
    account the [~pos] arguments:

    {[
      let b = execp (compile (seq [ start; str "a" ])) "aa" ~pos:1 in
      assert b
    ]} *)
val start : t

(** Final position. This is different from {!eos} in the way described
    in {!start}. *)
val stop : t

(** Word *)
val word : t -> t

(** Not at a word boundary *)
val not_boundary : t

(** Only matches the whole string, i.e. [fun t -> seq [ bos; t; eos ]]. *)
val whole_string : t -> t

(** {2 Match semantics}

    A regular expression frequently matches a string in multiple ways.  For
    instance [exec (compile (opt (str "a"))) "ab"] can match "" or "a". Match
    semantic can be modified with the functions below, allowing one to choose
    which of these is preferable.

    By default, the leftmost branch of alternations is preferred, and repetitions
    are greedy.

    Note that the existence of matches cannot be changed by specifying match
    semantics.  [seq [ bos; str "a"; non_greedy (opt (str "b")); eos ]] will
    match when applied to "ab". However if [seq [ bos; str "a"; non_greedy (opt
   (str "b")) ]] is applied to "ab", it will match "a" rather than "ab".

    Also note that multiple match semantics can conflict. In this case, the one
    executed earlier takes precedence. For instance, any match of [shortest (seq
   [ bos; group (rep (str "a")); group (rep (str "a")); eos ])] will always have
    an empty first group. Conversely, if we use [longest] instead of [shortest],
    the second group will always be empty. *)

(** Longest match semantics. That is, matches will match as many bytes as
    possible. If multiple choices match the maximum amount of bytes, the one
    respecting the inner match semantics is preferred. *)
val longest : t -> t

(** Same as {!longest}, but matching the least number of bytes. *)
val shortest : t -> t

(** First match semantics for alternations (not repetitions). That is, matches
    will prefer the leftmost branch of the alternation that matches the text. *)
val first : t -> t

(** Greedy matches for repetitions ({!opt}, {!rep}, {!rep1}, {!repn}): they will
    match as many times as possible. *)
val greedy : t -> t

(** Non-greedy matches for repetitions ({!opt}, {!rep}, {!rep1}, {!repn}): they
    will match as few times as possible. *)
val non_greedy : t -> t

(** {2 Groups (or submatches)} *)

(** Delimit a group. The group is considered as matching if it is used at least
    once (it may be used multiple times if is nested inside {!rep} for
    instance). If it is used multiple times, the last match is what gets
    captured. *)
val group : ?name:string -> t -> t

(** Remove all groups *)
val no_group : t -> t

(** When matching against [nest e], only the group matching in the
    last match of e will be considered as matching.

    For instance:
    {[
      let re = compile (rep1 (nest (alt [ group (str "a"); str "b" ]))) in
      let group = Re.exec re "ab" in
      assert (Group.get_opt group 1 = None);
      (* same thing but without [nest] *)
      let re = compile (rep1 (alt [ group (str "a"); str "b" ])) in
      let group = Re.exec re "ab" in
      assert (Group.get_opt group 1 = Some "a")
    ]} *)
val nest : t -> t

(** Mark a regexp. the markid can then be used to know if this regexp was used. *)
val mark : t -> Mark.t * t

(** {2 Character sets} *)

(** Any character of the string *)
val set : string -> t

(** Character ranges *)
val rg : char -> char -> t

(** Intersection of character sets *)
val inter : t list -> t

(** Difference of character sets *)
val diff : t -> t -> t

(** Complement of union *)
val compl : t list -> t

(** {2 Predefined character sets} *)

(** Any character *)
val any : t

(** Any character but a newline *)
val notnl : t

val alnum : t
val wordc : t
val alpha : t
val ascii : t
val blank : t
val cntrl : t
val digit : t
val graph : t
val lower : t
val print : t
val punct : t
val space : t
val upper : t
val xdigit : t

(** {2 Case modifiers} *)

(** Case sensitive matching. Note that this works on latin1, not ascii and not
    utf8. *)
val case : t -> t

(** Case insensitive matching. Note that this works on latin1, not ascii and not
    utf8. *)
val no_case : t -> t

(****)

(** {2 Internal debugging} *)

val pp : Format.formatter -> t -> unit
val pp_re : Format.formatter -> re -> unit

(** Alias for {!pp_re}. Deprecated *)
val print_re : Format.formatter -> re -> unit

(** {2 Experimental functions} *)

(** [witness r] generates a string [s] such that [execp (compile r) s] is true.

    Be warned that this function is buggy because it ignores zero-width
    assertions like beginning of words. As a result it can generate incorrect
    results. *)
val witness : t -> string

(** {2 Deprecated functions} *)

(** Alias for {!Group.t}. Deprecated *)
type substrings = Group.t [@@ocaml.deprecated "Use Group.t"]

(** Same as {!Group.get}. Deprecated *)
val get : Group.t -> int -> string
[@@ocaml.deprecated "Use Group.get"]

(** Same as {!Group.offset}. Deprecated *)
val get_ofs : Group.t -> int -> int * int
[@@ocaml.deprecated "Use Group.offset"]

(** Same as {!Group.all}. Deprecated *)
val get_all : Group.t -> string array
[@@ocaml.deprecated "Use Group.all"]

(** Same as {!Group.all_offset}. Deprecated *)
val get_all_ofs : Group.t -> (int * int) array
[@@ocaml.deprecated "Use Group.all_offset"]

(** Same as {!Group.test}. Deprecated *)
val test : Group.t -> int -> bool
[@@ocaml.deprecated "Use Group.test"]

(** Alias for {!Mark.t}. Deprecated *)
type markid = Mark.t [@@ocaml.deprecated "Use Mark."]

(** Same as {!Mark.test}. Deprecated *)
val marked : Group.t -> Mark.t -> bool
[@@ocaml.deprecated "Use Mark.test"]

(** Same as {!Mark.all}. Deprecated *)
val mark_set : Group.t -> Mark.Set.t
[@@ocaml.deprecated "Use Mark.all"]
