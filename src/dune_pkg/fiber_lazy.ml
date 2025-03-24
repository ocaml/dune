open Fiber.O

type 'a t =
  { value : 'a Fiber.Ivar.t
  ; mutable f : (unit -> 'a Fiber.t) option
  }

let create f = { f = Some f; value = Fiber.Ivar.create () }

let force t =
  let* () = Fiber.return () in
  match t.f with
  | None -> Fiber.Ivar.read t.value
  | Some f ->
    Fiber.of_thunk (fun () ->
      t.f <- None;
      let* v = f () in
      let+ () = Fiber.Ivar.fill t.value v in
      v)
;;
