open! Import

(** Ad-hoc feature flags for enabling various features for the dune
    developer preview. These should be false in the upstream version of
    dune and only manually set to true when building dune for the
    developer preview. If you find yourself enabling these when commiting
    to main, instead just remove the flag and permanently enable the
    feature. *)

(** Supposed to be enabled in the developer preview and disabled in the
    release, if you find yourself enabling this on release remove the
    check altogether *)
val use_autorelock : bool
