(** Encapsulates the situation where you have a configuration language made up
    of a sequence of terms (e.g. a list of directories to search for foreign
    header files), and want to add a new term to the language (include <path>)
    which parses a sexp list of terms in the same configuration language from
    the file at <path> and effectively replaces the (include ...) statement with
    the result of parsing the file. Supports chains of recursively included
    files, and detects include loops. *)

open! Import

(** The type of terms in the configuration language obtained by adding (include
    ...) statements to the base language *)
type 'a t

val of_base : 'a -> 'a t

val decode
  :  base_term:'a Dune_lang.Decoder.t
       (** The type of a term in the configuration language without (include
           ...) terms *)
  -> include_keyword:string
       (** The keyword that will be used to identify an include statement (ie.
           the "include" in (include ...)) *)
  -> include_allowed_in_versions:[ `Since of Syntax.Version.t | `All ]
       (** An expected use case for this module is adding (include ...)
           statements to existing configuration languages used in dune fields,
           and in such cases we'll want to assert that (include ...) statements
           are only used beyond a particular version of dune. An error will be
           throw during parsing if an (include ...) statement is encountered in
           versions of dune that don't satisfy this predicate. *)
  -> non_sexp_behaviour:[ `User_error | `Parse_as_base_term ]
       (** What to do if the included file doesn't contain a sexp *)
  -> 'a t Dune_lang.Decoder.t

(** Recursively expands (include ...) terms in the language, producing a list of
    terms in the original language (the language without (include ...)
    statements). Paths referred to by (include <path>) are resolved relative to
    [dir]. Paths are given as [String_with_vars.t], and the [expand_str]
    function is used to resolve them to strings. *)
val expand_include
  :  'a t
  -> expand:(String_with_vars.t -> Value.t Memo.t)
  -> dir:Path.t
  -> 'a list Memo.t
