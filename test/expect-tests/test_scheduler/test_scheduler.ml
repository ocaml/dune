open Stdune

type t = unit Fiber.Ivar.t Queue.t

let create () : t = Queue.create ()

let yield t =
  let ivar = Fiber.Ivar.create () in
  Queue.push t ivar;
  Fiber.Ivar.read ivar

exception Never

let run (t : t) fiber =
  Fiber.run fiber ~iter:(fun () ->
      match Queue.pop t with
      | None -> raise Never
      | Some e -> Fiber.Fill (e, ()))
