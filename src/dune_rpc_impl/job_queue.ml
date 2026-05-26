open Import

type 'a t =
  { queue : 'a Queue.t
  ; mutable reader : 'a Fiber.Ivar.t option
  }

let create () = { queue = Queue.create (); reader = None }
let pop_internal t = Queue.pop t.queue

let read t =
  Fiber.of_thunk (fun () ->
    match t.reader with
    | Some _ -> Code_error.raise "multiple concurrent reads of build job queue" []
    | None ->
      (match Queue.pop t.queue with
       | None ->
         let ivar = Fiber.Ivar.create () in
         t.reader <- Some ivar;
         Fiber.Ivar.read ivar
       | Some v -> Fiber.return v))
;;

let write t elem =
  Fiber.of_thunk (fun () ->
    match t.reader with
    | Some ivar ->
      t.reader <- None;
      Fiber.Ivar.fill ivar elem
    | None ->
      Queue.push t.queue elem;
      Fiber.return ())
;;
