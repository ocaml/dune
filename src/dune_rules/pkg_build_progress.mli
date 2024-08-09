open! Import

(** An [Action_builder.t] intended to be run concurrently with the
    build of a package that prints the build progress of that package
    to the console. This must be enabled by the setting the
    DUNE_CONFIG__PKG_BUILD_PROGRESS config variable to "enabled". If
    this variable is "disabled" (the default) then this function
    returns a no-op instead. *)
val display_build_progress
  :  Package.Name.t
  -> Package_version.t
  -> source_dir:[ `Some of Path.Build.t | `No_package_source ]
  -> target_dir:[ `Some of Path.Build.t | `No_build_or_install_command ]
  -> unit Action_builder.t
