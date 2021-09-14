open Types
open Exported_types

module Public : sig
  val ping : (unit, unit) Decl.Request.t

  val diagnostics : (unit, Diagnostic.t list) Decl.Request.t

  val shutdown : unit Decl.Notification.t

  val subscribe : Subscribe.t Decl.Notification.t

  val unsubscribe : Subscribe.t Decl.Notification.t

  val format_dune_file :
    (Path.t * [ `Contents of string ], string) Decl.Request.t

  val promote : (Path.t, unit) Decl.Request.t
end

module Server_side : sig
  val abort : Message.t Decl.Notification.t

  val log : Message.t Decl.Notification.t

  val progress : Progress.t Decl.Notification.t

  val diagnostic : Diagnostic.Event.t list Decl.Notification.t
end
