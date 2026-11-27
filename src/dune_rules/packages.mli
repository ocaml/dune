open Import

(* TODO : for now these take the super context, but eventually this should be
   more fine grained *)

(** Collect functions keyed by a package *)

val mlds : Super_context.t -> Package.Name.t -> Doc_sources.mld list Memo.t

(** Whether the documentation of the package is built. With [(odoc auto)], it
    is when the package is in [Dune_load.documented_packages]. [enabled_if]
    fields are ignored. A package that only contains an implementation of a
    virtual library is considered documented, although odoc skips such
    libraries. *)
val odoc_enabled : Package.t -> bool Memo.t
