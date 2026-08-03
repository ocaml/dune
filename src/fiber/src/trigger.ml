open Core

type t =
  { ivar : unit Ivar.t
  ; mutable filled : bool
  }

let create () = { ivar = Ivar.create (); filled = false }

let trigger' t =
  if t.filled
  then []
  else (
    t.filled <- true;
    [ Scheduler.Fill (t.ivar, ()) ])
;;

let trigger t =
  if t.filled
  then return ()
  else (
    t.filled <- true;
    Ivar.fill t.ivar ())
;;

let wait t = Ivar.read t.ivar
