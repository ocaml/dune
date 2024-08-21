(** Dune configuration library for non user facing configuration.

    The configuration available through this module is non user facing and is
    subject to change without warning.

    All configuration values have a name and can be configured either with
    [./configure --toggles name1,name2], or at runtime with
    the environment variable [DUNE_CONFIG__$NAME1=enabled/disabled] where
    [$NAME1] is the option's name in uppercase. *)

(* The code generating the call to this function is located in boot/configure. *)
val init : names:string list -> unit

(** Enable or disable the toolchains behaviour workaround.
    For more detail, see dune_rules/pkg_toolchains. *)
val toolchains : Config.Toggle.t Config.t

(** Enable or disable the displaying of package build progress.
    For more detail, see dune_rules/pkg_build_progress. *)
val pkg_build_progress : Config.Toggle.t Config.t
