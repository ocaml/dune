open Types
open Exported_types

module Public : sig
  val ping : (unit, unit) Decl.Request.t

  val diagnostics : (unit, Diagnostic.t list) Decl.Request.t

  val shutdown : unit Decl.Notification.t

  val subscribe : Subscribe.t Decl.Notification.t

  val unsubscribe : Subscribe.t Decl.Notification.t
end

module Internal : sig
  val build : (string list, Build_outcome.t) Decl.Request.t

  val build_decl : (string list, Build_outcome.t) Decl.Request.witness

  val status : (unit, Status.t) Decl.Request.t

  val status_decl : (unit, Status.t) Decl.Request.witness
end

module Server_side : sig
  val abort : Message.t Decl.Notification.t

  val log : Message.t Decl.Notification.t

  val progress : Progress.t Decl.Notification.t

  val diagnostic : Diagnostic.Event.t list Decl.Notification.t
end
