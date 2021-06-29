open Stdune
open Fiber.O

(* CR-someday amokhov: We could introduce another variant to represent the
   result of fiber evaluation, e.g. [Finished of 'a]. That would allow us to
   simplify some logic in Memo, specifically, remove [Sample_attempt.t]. *)
type 'a state =
  | Not_forced of { must_not_raise : unit -> 'a Fiber.t }
  | Forced of 'a Fiber.Ivar.t

type 'a t = { mutable state : 'a state }

(* If a given thunk does in fact raise an exception, forcing it will propagate
   the exception to the first caller, and leave all subsequent callers stuck,
   forever waiting for the unfilled [Ivar.t]. *)
let create ~must_not_raise = { state = Not_forced { must_not_raise } }

let force_impl ?on_blocking t =
  Fiber.of_thunk (fun () ->
      match t.state with
      | Forced ivar -> (
        match on_blocking with
        | None -> Fiber.Ivar.read ivar >>| Result.ok
        | Some on_blocking -> (
          let* res = Fiber.Ivar.peek ivar in
          match res with
          | Some res -> Fiber.return (Ok res)
          | None -> (
            on_blocking () >>= function
            | Ok () -> Fiber.Ivar.read ivar >>| Result.ok
            | Error _ as error -> Fiber.return error)))
      | Not_forced { must_not_raise } ->
        let ivar = Fiber.Ivar.create () in
        t.state <- Forced ivar;
        let* result = must_not_raise () in
        let+ () = Fiber.Ivar.fill ivar result in
        Ok result)

let force t =
  force_impl t >>| function
  | Ok res -> res
  | Error (_ : Nothing.t) -> .

let force_with_blocking_check t ~on_blocking = force_impl t ~on_blocking
