(*
   This file is extracted from the 0install library. It is distributed under
   the LGPL-2.1-or-later licence. See src/sat/COPYING.md for the full license.

   Copyright (C) 2013, Thomas Leonard
   See the README file for details, or visit http://0install.net.
*)

type t

val create : unit -> t
val is_empty : t -> bool
val add : t -> int -> unit
val mem : t -> int -> bool
val clear : t -> unit
