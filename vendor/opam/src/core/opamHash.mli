(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016-2017 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Stored as hexadecimal strings *)
type kind = [ `MD5 | `SHA256 | `SHA512 ]

type t

val kind: t -> kind

(** The value of the hash, as a string of hexadecimal characters *)
val contents: t -> string

val string_of_kind: kind -> string

val md5: string -> t
val sha256: string -> t
val sha512: string -> t

include OpamStd.ABSTRACT with type t := t

val of_string_opt: string -> t option

(** Check if [hash] contains only 0 *)
val is_null: t -> bool

(** returns a sub-path specific to this hash, e.g.
    "md5/d4/d41d8cd98f00b204e9800998ecf8427e", as a list *)
val to_path: t -> string list

(** Sorts the list from best hash (SHA512, ...) to weakest (..., MD5) *)
val sort : t list -> t list

val check_file: string -> t -> bool

(** Like [check_file], but returns the actual mismatching hash of the file, or
    [None] in case of match *)
val mismatch: string -> t -> t option

(** Compute hash of the given file *)
val compute: ?kind:kind -> string -> t

(** Compute the hash of the given string *)
val compute_from_string: ?kind:kind -> string -> t
