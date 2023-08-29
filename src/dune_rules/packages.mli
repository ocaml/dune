open Import

(* TODO : for now these take the super context, but eventually this should be
   more fine grained *)

(** Collect functions keyed by a package *)

val mlds_by_package
  :  Super_context.t
  -> Path.Build.t list Dune_lang.Package_name.Map.t Memo.t

val mlds : Super_context.t -> Package.Name.t -> Path.Build.t list Memo.t
