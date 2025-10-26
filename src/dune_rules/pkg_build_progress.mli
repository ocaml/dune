open Import

(** Formats a message in the style of a progress message. The text of
    the message will be "<verb> <object_>" with the verb colored to match
    the verb in progress messages (e.g. "Downloading", and the message
    will be left-padded so that the space between the verb and object_
    lines up with the spaces in progress messages, as long as the length
    of the verb does not exceed 12 characters.

    TODO(steve): unify this with the logic for printing build progress
    messages in dune_engine/process.ml *)
val format_user_message
  :  verb:string
  -> object_:User_message.Style.t Pp.t
  -> User_message.t

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
