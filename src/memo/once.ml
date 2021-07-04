open Stdune
open Fiber.O

type 'a state =
  | Not_started of { must_not_raise : unit -> 'a Fiber.t }
  | Running of 'a Fiber.Ivar.t
  | Finished of 'a

type 'a t = { mutable state : 'a state }

(* If a given thunk does in fact raise an exception, forcing it will propagate
   the exception to the first caller, and leave all subsequent callers stuck,
   forever waiting for the unfilled [Ivar.t]. *)
let create ~must_not_raise = { state = Not_started { must_not_raise } }

let force t ~on_blocking =
  Fiber.of_thunk (fun () ->
      match t.state with
      | Finished result -> Fiber.return (Ok result)
      | Running ivar -> (
        on_blocking () >>= function
        | Ok () -> Fiber.Ivar.read ivar >>| Result.ok
        | Error _ as error -> Fiber.return error)
      | Not_started { must_not_raise } ->
        let ivar = Fiber.Ivar.create () in
        t.state <- Running ivar;
        let* result = must_not_raise () in
        t.state <- Finished result;
        let+ () = Fiber.Ivar.fill ivar result in
        Ok result)
