(* For the purpose of avoiding dependency cycles between the rules and the
   engine.

   This name could probably be chosen to be a bit more informative. *)

open Import

type t = private
  { name : Context_name.t
  ; build_dir : Path.Build.t
  }

val create : name:Context_name.t -> t
val of_build_path : Path.Build.t -> t option
