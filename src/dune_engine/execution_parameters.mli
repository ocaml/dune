(** Parameters that influence rule execution *)

(** Such as:

    - should target be set read-only?

    - should aliases be expanded when sandboxing rules?

    These often depend on the version of the Dune language used, which is
    written in the [dune-project] file. Depending on the execution parameters
    rather than the whole [dune-project] file means that when some part of the
    [dune-project] file changes and this part does not have an effect on rule
    execution, we can skip a bunch of work by not trying to re-execute all the
    rules. *)

open Stdune

type t

val equal : t -> t -> bool

val hash : t -> int

val to_dyn : t -> Dyn.t

val make : dune_version:Dune_lang.Syntax.Version.t -> t

val dune_version : t -> Dune_lang.Syntax.Version.t

val should_remove_write_permissions_on_generated_files : t -> bool

val should_expand_aliases_when_sandboxing : t -> bool
