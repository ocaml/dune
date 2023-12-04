(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2016 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** (generated) Current OPAM version *)

include OpamStd.ABSTRACT

(** The current OPAM version *)
val current: t

(** Extracts the major version *)
val major: t -> t

(** Major+minor version, strips the patch version *)
val nopatch: t -> t

(** The current OPAM version, truncated (only MAJOR.MINOR) *)
val current_nopatch: t

(** The 'git' version of OPAM *)
val git: unit -> t option

(** Side-effect to set the git version later in the build *)
val set_git: string -> unit

(** [true] if this is a development version of opam *)
val is_dev_version : unit -> bool

(** The full version (current + git) *)
val full: unit -> t

(** Magic string, always of length 8 *)
val magic: unit -> string

(** Display the version message *)
val message: unit -> unit

(** Version comparison *)
val compare: t -> t -> int
