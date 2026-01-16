type t =
  { ivar : unit Fiber.Ivar.t
  ; mutable filled : bool
  }

let create () = { ivar = Fiber.Ivar.create (); filled = false }

let trigger t =
  if t.filled
  then []
  else (
    t.filled <- true;
    [ Fiber.Fill (t.ivar, ()) ])
;;

let wait t = Fiber.Ivar.read t.ivar
