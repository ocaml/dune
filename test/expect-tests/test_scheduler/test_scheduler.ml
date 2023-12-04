open Stdune

type job = Job : (unit -> 'a) * 'a Fiber.Ivar.t -> job
type t = job Queue.t

let create () : t = Queue.create ()

let yield t =
  Fiber.of_thunk
  @@ fun () ->
  let ivar = Fiber.Ivar.create () in
  Queue.push t (Job ((fun () -> ()), ivar));
  Fiber.Ivar.read ivar
;;

let yield_gen (t : t) ~do_in_scheduler =
  Fiber.of_thunk
  @@ fun () ->
  let ivar = Fiber.Ivar.create () in
  Queue.push t (Job (do_in_scheduler, ivar));
  Fiber.Ivar.read ivar
;;

exception Never

let run (t : t) fiber =
  Queue.clear t;
  Fiber.run fiber ~iter:(fun () ->
    match Queue.pop t with
    | None -> raise Never
    | Some (Job (job, ivar)) ->
      let v = job () in
      [ Fiber.Fill (ivar, v) ])
;;
