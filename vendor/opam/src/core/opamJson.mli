(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Json encoder; only needed for some debug options

    {b Warning.} Assumes given strings are UTF-8 encoded this is
    not checked by the module. *)

type t =
  [ `Null | `Bool of bool | `Float of float| `String of string
  | `A of t list | `O of (string * t) list ]

type 'a encoder = 'a -> t
type 'a decoder = t -> 'a option

val append: string -> t -> unit

val flush: out_channel -> unit

val to_string : ?minify:bool -> t -> string

val of_string: ?encoding:'a -> string -> t option
