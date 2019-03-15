(** Collect functions keyed by a package *)
(* TODO : for now these take the super context, but eventually this should be
   more fine grained *)
open Stdune

val mlds : Super_context.t -> Package.Name.t -> Path.t list
