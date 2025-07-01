open Import

(* TODO : for now these take the super context, but eventually this should be
   more fine grained *)

(** Collect functions keyed by a package *)

val mlds : Super_context.t -> Package.Name.t -> Doc_sources.mld list Memo.t
