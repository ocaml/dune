(* For the purpose of avoiding dependency cycles between the rules and the
   engine.

   This name could probably be chosen to be a bit more informative. *)

open! Stdune
open! Import

type t = private
  { name : Context_name.t
  ; build_dir : Path.Build.t
  ; host : Context_name.t option
  }

val create : name:Context_name.t -> host:Context_name.t option -> t
