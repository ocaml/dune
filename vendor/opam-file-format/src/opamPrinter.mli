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

(** Functions for converting parsed opam files back to strings *)


(** [OpamPrinter] transitional module with full position types *)
module FullPos : sig

  open OpamParserTypes.FullPos

  (** {2 Printers for the [value] and [opamfile] formats} *)

  val relop_kind: [< relop_kind ] -> string
  (** Converts {!OpamParserTypes.FullPos.relop_kind} to its string representation
      ([=], [!=], ..., [~]). *)

  val logop_kind: [< logop_kind ] -> string
  (** Converts {!OpamParserTypes.FullPos.logop_kind} to its string representation
      ([&] and [|]). *)

  val pfxop_kind: [< pfxop_kind ] -> string
  (** Converts {!OpamParserTypes.FullPos.logop_kind} to its string representation
      ([&] and [|]). *)

  val env_update_op_kind: env_update_op_kind -> string
  (** Converts {!OpamParserTypes.FullPos.env_update_op_kind} to its string representation
      ([=], [+=], ..., [=:]). *)


  val relop: relop -> string
  (** Converts {!OpamParserTypes.FullPos.relop} to its string representation
      ([=], [!=], ..., [~]). *)

  val logop: logop -> string
  (** Converts {!OpamParserTypes.FullPos.logop} to its string representation
      ([&] and [|]). *)

  val pfxop: pfxop -> string
  (** Converts {!OpamParserTypes.FullPos.pfxop} to its string representation
      ([!] and [?]). *)

  val env_update_op: env_update_op -> string
  (** Converts {!OpamParserTypes.FullPos.env_update_op} to its string representation
      ([=], [+=], ..., [=:]). *)


  val value : value -> string
  (** Converts {!value} to a string {b always using LF-encoding of newlines}. *)

  val value_list: value list with_pos -> string
  (** Converts a list of {!value}s to a string {b always using LF-encoding of
      newlines}. *)

  val items: opamfile_item list -> string
  (** Converts a list of opam field/sections to a string.

      @raise Invalid_argument if ["opam-version"] is greater than "2.0"
                              and not solely the first item. *)

  val opamfile: opamfile -> string
  (** Converts an {!opamfile} to a string.

      @raise Invalid_argument if ["opam-version"] is greater than "2.0"
                              and not solely the first item. *)

  val format_opamfile: Format.formatter -> opamfile -> unit
  (** Writes an {!opamfile} to a [Format.formatter]. The function ensures that all
      newlines are sent using [Format]'s break instructions (and so ultimately are
      processed with the [out_newline] function of the formatter) but it is the
      responsibility of the caller to ensure that the formatter is configured for
      the required output, if necessary.

      @raise Invalid_argument if ["opam-version"] is greater than "2.0"
                              and not solely the first item. *)

  (** {2 Normalised output for opam syntax files} *)

  (** opam normalised file format, for signatures.

      - each top-level field on a single line
      - newlines are LF-encoded (including on Windows)
      - file ends with a newline
      - spaces only after [fieldname:], between elements in lists, before braced
          options, between operators and their operands
      - fields are sorted lexicographically by field name
          (using [String.compare])
      - newlines in strings turned to ['\n'], backslashes and double quotes
          escaped
      - no comments (they don't appear in the internal file format anyway)
      - fields containing an empty list, or a singleton list containing an empty
          list, are not printed at all
  *)
  module Normalise : sig
    val escape_string : string -> string
    val value : value -> string
    val item : opamfile_item -> string
    val item_order : opamfile_item -> opamfile_item -> int
    val items : opamfile_item list -> string
    val opamfile : opamfile -> string
  end

  (** {2 Format-preserving reprinter} *)

  module Preserved : sig

    val items: string -> opamfile_item list -> opamfile_item list -> string
    (** [items str orig_its its] converts [its] to string, while attempting to
        preserve the layout and comments of the original [str] for unmodified
        elements. The function assumes that [str] parses to the items
        [orig_its].

        @raise Invalid_argument if ["opam-version"] is greater than "2.0"
                                and not solely the first item in either list. *)

    val opamfile: ?format_from:file_name -> opamfile -> string
    (** [opamfile f] converts [f] to string, respecting the layout and comments in
        the corresponding on-disk file for unmodified items. [format_from] can be
        specified instead of using the filename specified in [f].

        @raise Invalid_argument if ["opam-version"] is greater than "2.0"
                                and not solely the first item in the list. Note that
                                any errors in the file raise {!OpamLexer.Error} as
                                normal. *)
  end

  (** {2 Random utility functions} *)

  val value_equals: value -> value -> bool
  (** Compares structurally, without considering file positions *)

  val opamfile_item_equals: opamfile_item -> opamfile_item -> bool
  (** Compares structurally, without considering file positions *)

end

open OpamParserTypes

val relop: [< relop ] -> string
[@@ocaml.deprecated "Use OpamPrinter.FullPos.relop instead."]

val logop: [< logop ] -> string
[@@ocaml.deprecated "Use OpamPrinter.FullPos.logop instead."]

val pfxop: [< pfxop ] -> string
[@@ocaml.deprecated "Use OpamPrinter.FullPos.pfxop instead."]

val env_update_op: env_update_op -> string
[@@ocaml.deprecated "Use OpamPrinter.FullPos.env_update_op instead."]

val value : value -> string
[@@ocaml.deprecated "Use OpamPrinter.FullPos.value instead."]

val value_list: value list -> string
[@@ocaml.deprecated "Use OpamPrinter.FullPos.value_list instead."]

val items: opamfile_item list -> string
[@@ocaml.deprecated "Use OpamPrinter.FullPos.items instead."]

val opamfile: opamfile -> string
[@@ocaml.deprecated "Use OpamPrinter.FullPos.opamfile instead."]

val format_opamfile: Format.formatter -> opamfile -> unit
[@@ocaml.deprecated "Use OpamPrinter.FullPos.format_opamfile instead."]

module Normalise : sig
  val escape_string : string -> string
  val value : value -> string
  val item : opamfile_item -> string
  val item_order : opamfile_item -> opamfile_item -> int
  val items : opamfile_item list -> string
  val opamfile : opamfile -> string
end
[@@ocaml.deprecated "Use OpamPrinter.FullPos.Normalise instead."]

module Preserved : sig
  val items: string -> opamfile_item list -> opamfile_item list -> string

  val opamfile: ?format_from:file_name -> opamfile -> string
end
[@@ocaml.deprecated "Use OpamPrinter.FullPos.Preserved instead."]


val value_equals: value -> value -> bool
[@@ocaml.deprecated "Use OpamPrinter.FullPos.value_equals instead."]

val opamfile_item_equals: opamfile_item -> opamfile_item -> bool
[@@ocaml.deprecated "Use OpamPrinter.FullPos.opamfile_item_equals instead."]

