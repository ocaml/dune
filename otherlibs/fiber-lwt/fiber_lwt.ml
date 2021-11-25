open Stdune

module Fiber_inside_lwt = struct
  let key = Fiber.Var.create ()

  let run fiber =
    let fills, push = Lwt_stream.create () in
    let fiber = Fiber.Var.set key push (fun () -> fiber) in
    let rec loop = function
      | Fiber.Scheduler.Done x -> Lwt.return x
      | Fiber.Scheduler.Stalled stalled ->
        Lwt.bind (Lwt_stream.next fills) (fun fill ->
            loop (Fiber.Scheduler.advance stalled [ fill ]))
    in
    loop (Fiber.Scheduler.start fiber)

  let callback_to_lwt f =
    Fiber.bind (Fiber.Var.get key) ~f:(function
      | None ->
        failwith "Fiber_lwt.Fiber_inside_lwt.run_lwt: called outside [run]"
      | Some push_fill ->
        let ivar = Fiber.Ivar.create () in
        Lwt.async (fun () ->
            Lwt.bind
              (Lwt.try_bind f
                 (fun x -> Lwt.return (Ok x))
                 (fun exn -> Lwt.return (Error exn)))
              (fun x ->
                push_fill (Some (Fiber.Fill (ivar, x)));
                Lwt.return_unit));
        Fiber.Ivar.read ivar)
end
