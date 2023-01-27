open Types

type 'a t =
  { poll : (Id.t, 'a option) Decl.Request.witness
  ; cancel : Id.t Decl.Notification.witness
  }

let of_procedure p =
  let open Procedures.Poll in
  { poll = (poll p).decl; cancel = (cancel p).decl }

let poll t = t.poll

let poll_cancel t = t.cancel
