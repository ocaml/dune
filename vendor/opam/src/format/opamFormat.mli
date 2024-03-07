(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** OPAM files syntax and conversion tools *)

open OpamParserTypes.FullPos
open OpamTypes
open OpamPp

(** Get the position out of a value *)
val value_pos: value -> pos

(** {3 low-level Pps for the Lines parser ([string list list])} *)

type lines = string list list

(** Provided an empty element, addition and fold operations with signatures as
    per Set.S, and a pp from lines to elements, returns a pp parsing from
    lines *)
val lines_set:
  empty:'set ->
  add:('elt -> 'set -> 'set) ->
  fold:(('elt -> lines -> lines) -> 'set -> lines -> lines) ->
  (string list, 'elt) t ->
  (lines, 'set) t

(** Provided an empty element, addition and fold operations with signatures as
    per Map.S, and a pp from lines to key, value pairs, returns a pp parsing
    from lines *)
val lines_map :
  empty:'map ->
  add:('k -> 'v -> 'map -> 'map) ->
  fold:(('k -> 'v -> lines -> lines) -> 'map -> lines -> lines) ->
  (string list, 'k * 'v) t ->
  (lines, 'map) t

(** {3 Pps for the type [value], used by opam-syntax files ([opamfile])} *)

module V : sig
  (** These base converters raise [Unexpected] when not run on the right input
      (which is then converted to [Bad_format] by the parser. *)

  val bool : (value, bool) t
  val int : (value, int) t

  (** positive or null integer *)
  val pos_int : (value, int) t

  val ident : (value, string) t
  val string : (value, string) t

  (** Trimmed string *)
  val string_tr : (value, string) t

  (** Command arguments, i.e. strings or idents *)
  val simple_arg : (value, simple_arg) t

  (** Strings or bools *)
  val variable_contents : (value, variable_contents) t

  (** "[a b c]"; also allows just "a" to be parsed as a singleton list *)
  val list : (value, value list) t

  (** "(a b c)" *)
  val group : (value, value list) t

  (** Options in the [value] type sense, i.e. a value with an optional list
      of parameters in braces: ["value {op1 op2}"] *)
  val option :
    (value, value * value list) t

  val map_group : (value, 'a) t -> (value, 'a list) t

  (** An expected list depth may be specified to enable removal of extra
      brackets (never use [~depth] for an inner list) *)
  val map_list : ?depth:int -> (value, 'a) t -> (value, 'a list) t

  (** Normalises to the given list depth when parsing, and removes brackets
      that can be made implicit when printing *)
  val list_depth : int -> (value, value) t

  (** Maps on the two terms of an option constructor. *)
  val map_option : (value, 'a) t -> (value list, 'b) t -> (value, 'a * 'b) t

  (** Maps over two options (e.g. [v {op1} {op2}]) *)
  val map_options_2 :
    (value, 'a) t -> (value list, 'b) t -> (value list, 'c) t ->
    (value, 'a * 'b * 'c) t

  (** Maps over three options (e.g. [v {op1} {op2} {op3}]) *)
  val map_options_3 :
    (value, 'a) t ->
    (value list, 'b) t -> (value list, 'c) t -> (value list, 'd) t ->
    (value, 'a * 'b * 'c * 'd) t

  (** A pair is simply a list with two elements in the [value] type *)
  val map_pair :
    (value, 'a) t ->
    (value, 'b) t -> (value, 'a * 'b) t

  (** A triple is simply a list with three elements in the [value] type *)
  val map_triple :
    (value, 'a) t ->
    (value, 'b) t ->
    (value, 'c) t -> (value, 'a * 'b * 'c) t

  val url : (value, url) t

  (** Specialised url parser when the backend is already known *)
  val url_with_backend : OpamUrl.backend -> (value, url) t

  val compiler_version : (value, string) t

  val filter_ident :
    (value,
     name option list * variable *
     (string * string) option)
      t

  val filter : (value list, filter) t

  (** Arguments in commands (term + optional filter) *)
  val arg : (value, simple_arg * filter option) t

  val command : (value, (simple_arg * filter option) list * filter option) t

  (** Simple dependency constraints *)
  val constraints :
    (value, 'a) t ->
    (value list, (OpamFormula.relop * 'a) OpamFormula.formula) t

  (** Dependency constraints mixed with filters *)
  val filtered_constraints :
    (value, 'version) t ->
    (value list, 'version filter_or_constraint OpamFormula.formula) t

  (** Package versions *)
  val version: (value, version) t

  (** Package versions as filters, as they may appear in dependency (may be an
      expanded string or an ident) *)
  val ext_version: (value, filter) t

  (** A package name, encoded as a string, but with restrictions *)
  val pkgname: (value, name) t

  (** Returns an atom parser [("package" {>= "version"})] from a constraint
      and a version parser*)
  val package_atom:
    (value list, 'a) t -> (value, name * 'a) t

  (** Takes a parser for constraints. Lists without operator will be
      understood as conjunctions or disjunctions depending on the first
      argument. *)
  val package_formula :
    [< `Conj | `Disj ] ->
    (value list, 'a) t ->
    (value, (name * 'a) OpamFormula.formula) t

  (** Like [package_formula], but takes the list items directly *)
  val package_formula_items :
    [< `Conj | `Disj ] ->
    (value list, 'a) t ->
    (value list, (name * 'a) OpamFormula.formula) t

  (** Environment variable updates syntax *)
  val env_binding : (value, env_update) t

  val os_constraint : (value, (bool * string) OpamFormula.formula) t
end

(** {3 Specific Pps for items lists and fields (opamfile)} *)

module I :
sig
  val file : (opamfile, filename * opamfile_item list) t

  val map_file : (opamfile_item list, 'a) t -> (opamfile, filename * 'a) t

  val item : (opamfile_item, string * value) t

  val items : (opamfile_item list, (string * value) list) t

  (** Suitable for the [fields] [sections] argument, when the sections are
      anonymous ([section_name = None]) *)
  val anonymous_section : ('a, 'b) t -> ((string option * 'a) list, 'b) t

  type ('a, 'value) fields_def = (string * ('a, 'value) field_parser) list

  (** Parses an item list into a record using a fields_def; errors in a field
      cause the field to be ignored, and are aggregated into the returned
      [field, bad_format] list. Errors are ignored when printing back. *)
  val fields :
    ?name:string ->
    empty:'a ->
    ?sections:
      ('a, (string option * (opamfile_item list)) list) fields_def ->
    ?mandatory_fields:string list ->
    ('a, value) fields_def ->
    (opamfile_item list, 'a * (string * bad_format) list) t

  (** Intended to be piped after [fields]. If the errors list is non-empty, this
      raises [Bad_format_list] if [strict], and otherwise prints warnings for
      all the errors. The errors are then dropped when parsing, and initialised
      to empty when printing. [strict] is taken from the global settings if
      unspecified. [condition] may be added to only show the errors when it
      returns [true], and only log them otherwise. *)
  val show_errors :
    ?name:string ->
    ?strict:bool ->
    ?condition:('a -> bool) ->
    unit ->
    ('a * (string * bad_format) list, 'a) t

  (** Intended to be piped after [fields], this processes the given function on
      the errors, then drops them when parsing. When printing, just sets empty
      errors. *)
  val on_errors :
    ?name:string -> ('a -> string * bad_format -> 'a) ->
    ('a * (string * bad_format) list, 'a) t

  (** Partitions items in an opamfile base on a condition on the variable
      names. If a section is encountered, it is kept in the second list (as
      filter returning false), unless [section] is true. *)
  val partition_fields :
    ?section:bool -> (string -> bool) ->
    (opamfile_item list, opamfile_item list * opamfile_item list) t

  (** Partitions items in an opamfile base on a generic condition on the
      items *)
  val partition :
    (opamfile_item -> bool) ->
    (opamfile_item list, opamfile_item list * opamfile_item list) t

  (** Parse a single field from a file, return the result and the unchanged
      item list. The single field is ignored when printing back. *)
  val field :
    string ->
    (pos:pos -> value -> 'a) ->
    (opamfile_item list, 'a option * opamfile_item list) t

  (** Parse a single section with the given "kind", towards its name and
      contents *)
  val section :
    string ->
    (opamfile_item, (string option * opamfile_item list)) t

  (** Extracts a single item with the given variable name from an item list.
      The item is removed from the returned item list, and the two are
      re-combined when printing *)
  val extract_field :
    string ->
    (opamfile_item list, value option * opamfile_item list) t

  (** Checks the [opam_version] field; otherwise the identity *)
  val check_opam_version :
    ?optional:bool -> format_version:opam_version -> ?f:(opam_version -> bool)
    -> unit ->
    (opamfile_item list, opamfile_item list) t

  (** Add [opam-version] field printing at printing, and removes it from parsed
      fields if [undefined] (default is false) *)
  val opam_version :
    ?undefined:bool -> format_version:opam_version -> unit ->
    (opamfile_item list, opamfile_item list) t

  (** Signature handling (wip) *)

  (** A signature is a keyid, an algorithm and the signature proper *)
  type signature = string * string * string

  val signature : (value, signature) t

  exception Invalid_signature of
      pos * (string * string * string) list option

  (** Pp for signed files. Will assert fail if attempting to write a file with
      an invalid signature. *)
  val signed:
    check:(signature list -> string -> bool) ->
    (opamfile_item list, signature list * opamfile_item list) t
end
