(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  linking exception.                                                 *)
(*                                                                     *)
(***********************************************************************)

(* $Id: re_str.mli,v 1.1 2002/01/16 14:16:04 vouillon Exp $ *)

(** Module [Str]: regular expressions and high-level string processing *)

(** {2 Regular expressions} *)

(** The type of compiled regular expressions. *)
type regexp

(** Compile a regular expression. The syntax for regular expressions
    is the same as in Gnu Emacs. The special characters are
    [$^.*+?[]]. The following constructs are recognized:
    - [.     ] matches any character except newline
    - [*     ] (postfix) matches the previous expression zero, one or
      several times
    - [+     ] (postfix) matches the previous expression one or
      several times
    - [?     ] (postfix) matches the previous expression once or
      not at all
    - [[..]  ] character set; ranges are denoted with [-], as in [[a-z]];
      an initial [^], as in [[^0-9]], complements the set
    - [^     ] matches at beginning of line
    - [$     ] matches at end of line
    - [\|    ] (infix) alternative between two expressions
    - [\(..\)] grouping and naming of the enclosed expression
    - [\1    ] the text matched by the first [\(...\)] expression
      ([\2] for the second expression, etc)
    - [\b    ] matches word boundaries
    - [\     ] quotes special characters. *)
val regexp : string -> regexp

(** Same as [regexp], but the compiled expression will match text
    in a case-insensitive way: uppercase and lowercase letters will
    be considered equivalent. *)
val regexp_case_fold : string -> regexp

(** [Str.quote s] returns a regexp string that matches exactly
    [s] and nothing else. *)
val quote : string -> string

(** [Str.regexp_string s] returns a regular expression
    that matches exactly [s] and nothing else. *)
val regexp_string : string -> regexp

(** [Str.regexp_string_case_fold] is similar to [Str.regexp_string], but the regexp
    matches in a case-insensitive way. *)
val regexp_string_case_fold : string -> regexp

(** {2 String matching and searching} *)

(** [string_match r s start] tests whether the characters in [s]
    starting at position [start] match the regular expression [r].
    The first character of a string has position [0], as usual. *)
val string_match : regexp -> string -> int -> bool

(** [search_forward r s start] searches the string [s] for a substring
    matching the regular expression [r]. The search starts at position
    [start] and proceeds towards the end of the string.
    Return the position of the first character of the matched
    substring, or raise [Not_found] if no substring matches. *)
val search_forward : regexp -> string -> int -> int

(** Same as [search_forward], but the search proceeds towards the
    beginning of the string. *)
val search_backward : regexp -> string -> int -> int

(** Similar to [string_match], but succeeds whenever the argument
    string is a prefix of a string that matches.  This includes
    the case of a true complete match. *)
val string_partial_match : regexp -> string -> int -> bool

(** [matched_string s] returns the substring of [s] that was matched
    by the latest [string_match], [search_forward] or [search_backward].
    The user must make sure that the parameter [s] is the same string
    that was passed to the matching or searching function. *)
val matched_string : string -> string

(** [match_beginning ()] returns the position of the first character
    of the substring that was matched by [string_match],
    [search_forward] or [search_backward]. *)
val match_beginning : unit -> int

(** [match_end ()] returns the position of the character following the
    last character of the substring that was matched by [string_match],
    [search_forward] or [search_backward]. *)
val match_end : unit -> int

(** [matched_group n s] returns the substring of [s] that was matched
    by the [n]th group [\(...\)] of the regular expression during
    the latest [string_match], [search_forward] or [search_backward].
    The user must make sure that the parameter [s] is the same string
    that was passed to the matching or searching function.
    [matched_group n s] raises [Not_found] if the [n]th group
    of the regular expression was not matched.  This can happen
    with groups inside alternatives [\|], options [?]
    or repetitions [*].  For instance, the empty string will match
    [\(a\)*], but [matched_group 1 ""] will raise [Not_found]
    because the first group itself was not matched. *)
val matched_group : int -> string -> string

(** [group_beginning n] returns the position of the first character
    of the substring that was matched by the [n]th group of the regular expression.
    Raises [Not_found] if the [n]th group of the regular expression was not matched. *)
val group_beginning : int -> int

(** [group_end n] returns the position of the character following
    the last character of the matched substring.
    Raises [Not_found] if the [n]th group of the regular expression was not matched. *)
val group_end : int -> int

(** {2 Replacement} *)

(** [global_replace regexp templ s] returns a string identical to [s],
    except that all substrings of [s] that match [regexp] have been
    replaced by [templ]. The replacement template [templ] can contain
    [\1], [\2], etc; these sequences will be replaced by the text
    matched by the corresponding group in the regular expression.
    [\0] stands for the text matched by the whole regular expression. *)
val global_replace : regexp -> string -> string -> string

(** Same as [global_replace], except that only the first substring
    matching the regular expression is replaced. *)
val replace_first : regexp -> string -> string -> string

(** [global_substitute regexp subst s] returns a string identical
    to [s], except that all substrings of [s] that match [regexp]
    have been replaced by the result of function [subst]. The
    function [subst] is called once for each matching substring,
    and receives [s] (the whole text) as argument. *)
val global_substitute : regexp -> (string -> string) -> string -> string

(** Same as [global_substitute], except that only the first substring
    matching the regular expression is replaced. *)
val substitute_first : regexp -> (string -> string) -> string -> string

(** [replace_matched repl s] returns the replacement text [repl]
    in which [\1], [\2], etc. have been replaced by the text
    matched by the corresponding groups in the most recent matching
    operation.  [s] must be the same string that was matched during
    this matching operation. *)
val replace_matched : string -> string -> string

(** {2 Splitting} *)

(** [split r s] splits [s] into substrings, taking as delimiters
    the substrings that match [r], and returns the list of substrings.
    For instance, [split (regexp "[ \t]+") s] splits [s] into
    blank-separated words.  An occurrence of the delimiter at the
    beginning and at the end of the string is ignored. *)
val split : regexp -> string -> string list

(** Same as [split], but splits into at most [n] substrings,
    where [n] is the extra integer parameter. *)
val bounded_split : regexp -> string -> int -> string list

(** Same as [split], but occurrences of the delimiter at the beginning
    and at the end of the string are recognized and returned as empty strings
    in the result.
    For instance, [split_delim (regexp " ") " abc "] returns [[""; "abc"; ""]],
    while [split] with the same arguments returns [["abc"]]. *)
val split_delim : regexp -> string -> string list

(** Same as [bounded_split] and [split_delim], but occurrences of
    the delimiter at the beginning and at the end of the string are recognized
    and returned as empty strings in the result.
    For instance, [split_delim (regexp " ") " abc "] returns [[""; "abc"; ""]],
    while [split] with the same arguments returns [["abc"]]. *)
val bounded_split_delim : regexp -> string -> int -> string list

type split_result =
  | Text of string
  | Delim of string

(** Same as [split_delim], but returns the delimiters
    as well as the substrings contained between delimiters.
    The former are tagged [Delim] in the result list;
    the latter are tagged [Text].
    For instance, [full_split (regexp "[{}]") "{ab}"] returns
    [[Delim "{"; Text "ab"; Delim "}"]]. *)
val full_split : regexp -> string -> split_result list

(** Same as [split_delim] and [bounded_split_delim], but returns
    the delimiters as well as the substrings contained between delimiters.
    The former are tagged [Delim] in the result list;
    the latter are tagged [Text].
    For instance, [full_split (regexp "[{}]") "{ab}"] returns
    [[Delim "{"; Text "ab"; Delim "}"]]. *)
val bounded_full_split : regexp -> string -> int -> split_result list

(** {2 Extracting substrings} *)

(** [string_before s n] returns the substring of all characters of [s]
    that precede position [n] (excluding the character at
    position [n]). *)
val string_before : string -> int -> string

(** [string_after s n] returns the substring of all characters of [s]
    that follow position [n] (including the character at
    position [n]). *)
val string_after : string -> int -> string

(** [first_chars s n] returns the first [n] characters of [s].
    This is the same function as [string_before]. *)
val first_chars : string -> int -> string

(** [last_chars s n] returns the last [n] characters of [s]. *)
val last_chars : string -> int -> string
