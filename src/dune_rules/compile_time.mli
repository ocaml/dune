open Import

(** Dune configuration library for non user facing configuration.

    The configuration available through this module is non user facing and is
    subject to change without warning.

    All configuration values have a name and can be configured either
    - at build time with [./configure --enable-<name-of-option>],
    - or at runtime with the environment variable [DUNE_CONFIG__$NAME=enabled/disabled]
      where [$NAME] is the option's name in uppercase. *)

(** Enable or disable the toolchains behaviour workaround.
    For more detail, see src/dune_rules/pkg_toolchains.mli *)
val toolchains : Config.Toggle.t Config.t

(** Enable or disable using package management to install dev tools. *)
val lock_dev_tools : Config.Toggle.t Config.t

val bin_dev_tools : Config.Toggle.t Config.t
val portable_lock_dir : Config.Toggle.t Config.t
