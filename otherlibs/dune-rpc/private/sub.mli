open Types

type 'a t

val of_procedure : 'a Procedures.Poll.t -> 'a t

val poll : 'a t -> (Id.t, 'a option) Decl.Request.witness

val poll_cancel : 'a t -> Id.t Decl.Notification.witness
