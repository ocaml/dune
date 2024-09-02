open! Import

(** An action which prints a progress message about a package to
    the console so users can be informed about which of their
    project's dependencies are currently being installed.

    The message will only print if the
    DUNE_CONFIG__PKG_BUILD_PROGRESS config variable is "enabled"
    (it's "disabled" by default). *)
val progress_action
  :  Package.Name.t
  -> Package_version.t
  -> [ `Downloading | `Building ]
  -> Action.t
