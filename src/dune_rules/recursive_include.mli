(** Encapsulates the situation where you have a configuration language made up
    of a sequence of terms (e.g. a list of directories to search for foreign
    header files), and want to add a new term to the language (include <path>)
    which parses a sexp list of terms in the same configuration language from
    the file at <path> and effectively replaces the (include ...) statement with
    the result of parsing the file. Supports chains of recursively included
    files, and detects include loops. *)

open! Import

module Make (Base_term : sig
  (** The type of a term in the configuration language without (include ...)
      terms *)
  type t

  val decode : t Dune_lang.Decoder.t
end) (_ : sig
  (** The keyword that will be used to identify an include statement (ie. the
      "include" in (include ...)) *)
  val include_keyword : string

  (** An expected use case for this module is adding (include ...) statements to
      existing configuration languages used in dune fields, and in such cases
      we'll want to assert that (include ...) statements are only used beyond a
      particular version of dune. An error will be throw during parsing if an
      (include ...) statement is encountered in versions of dune that don't
      satisfy this predicate. *)
  val include_allowed_in_versions : [ `Since of Syntax.Version.t | `All ]

  (** What to do if the included file doesn't contain a sexp *)
  val non_sexp_behaviour : [ `User_error | `Parse_as_base_term ]
end) : sig
  (** The type of terms in the configuration language obtained by adding
      (include ...) statements to the base language *)
  type t

  val of_base : Base_term.t -> t

  val decode : t Dune_lang.Decoder.t

  (** Recursively expands (include ...) terms in the language, producing a list
      of terms in the original language (the language without (include ...)
      statements). Paths referred to by (include <path>) are resolved relative
      to [dir]. Paths are given as [String_with_vars.t], and the [expand_str]
      function is used to resolve them to strings. *)
  val expand_include :
       t
    -> expand_str:(String_with_vars.t -> string Memo.t)
    -> dir:Path.Build.t
    -> Base_term.t list Memo.t
end
