(**************************************************************************)
(*                                                                        *)
(*    Copyright 2022 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* Software Heritage Identifiers *)
type t

include OpamStd.ABSTRACT with type t := t
val hash : t -> string

(** Check url validity regarding its form:
    http backend and swhid path prefix [swhid.opam.ocaml.org] *)
val is_valid: OpamUrl.t -> bool

(** Extract an swhid from an url *)
val of_url: OpamUrl.t -> t option

(** Create an opam url from an swhid, in the form of
    https://swhid.opam.ocaml.org/swhi *)
val to_url: t -> OpamUrl.t

(** Compute and SWH identifier from the given directory *)
val compute: OpamFilename.Dir.t -> string option
