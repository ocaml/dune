(* TODO : for now these take the super context, but eventually this should be
   more fine grained *)

(** Collect functions keyed by a package *)
open Stdune

val mlds : Super_context.t -> Package.Name.t -> Path.Build.t list
