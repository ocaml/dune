(* This file isn't actually needed by the bootstrap process.

   It is here to guard against accidental updates to [libs.ml]. If there's an
   interface mismatch error, it means that the changes to [libs.ml] must be
   undone and bootstrap to be done again.
*)

open Types

val external_libraries : string list
val local_libraries : string library list
val main : string library
