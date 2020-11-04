(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** {2 Printers for the [value] and [opamfile] formats} *)

open OpamParserTypes

val relop: [< relop ] -> string

val logop: [< logop ] -> string

val pfxop: [< pfxop ] -> string

val env_update_op: env_update_op -> string

val value : value -> string

val value_list: value list -> string

val items: opamfile_item list -> string

val opamfile: opamfile -> string

val format_opamfile: Format.formatter -> opamfile -> unit

(** {2 Normalised output for opam syntax files} *)

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
  (** [items str orig_its its] converts [its] to string, while attempting to
      preserve the layout and comments of the original [str] for unmodified
      elements. The function assumes that [str] parses to the items
      [orig_its]. *)
  val items: string -> opamfile_item list -> opamfile_item list -> string

  (** [opamfile f] converts [f] to string, respecting the layout and comments in
      the corresponding on-disk file for unmodified items. [format_from] can be
      specified instead of using the filename specified in [f]. *)
  val opamfile: ?format_from:file_name -> opamfile -> string
end

(** {2 Random utility functions} *)

(** Compares structurally, without considering file positions *)
val value_equals: value -> value -> bool

(** Compares structurally, without considering file positions *)
val opamfile_item_equals: opamfile_item -> opamfile_item -> bool
