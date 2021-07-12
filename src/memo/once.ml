open Stdune
open Fiber.O

type 'a state =
  | Not_forced of { must_not_raise : unit -> 'a Fiber.t }
  | Forced of 'a Fiber.Ivar.t

type 'a t = { mutable state : 'a state }

(* If a given thunk does in fact raise an exception, forcing it will propagate
   the exception to the first caller, and leave all subsequent callers stuck,
   forever waiting for the unfilled [Ivar.t]. *)
let create ~must_not_raise = { state = Not_forced { must_not_raise } }

let force t ~on_blocking =
  Fiber.of_thunk (fun () ->
      match t.state with
      | Forced ivar -> (
        let* res = Fiber.Ivar.peek ivar in
        match res with
        | Some res -> Fiber.return (Ok res)
        | None -> (
          on_blocking () >>= function
          | Ok () -> Fiber.Ivar.read ivar >>| Result.ok
          | Error _ as error -> Fiber.return error))
      | Not_forced { must_not_raise } ->
        let ivar = Fiber.Ivar.create () in
        t.state <- Forced ivar;
        let* result = must_not_raise () in
        let+ () = Fiber.Ivar.fill ivar result in
        Ok result)
