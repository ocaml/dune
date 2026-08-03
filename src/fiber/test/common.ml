open Stdune
open Fiber.O
open Dyn

let printf = Printf.printf
let print_dyn dyn = Dyn.to_string dyn |> print_endline
let () = Printexc.record_backtrace false

module Scheduler = struct
  let t = Test_scheduler.create ()
  let yield () = Test_scheduler.yield t
  let run f = Test_scheduler.run t f
end

let failing_fiber () : unit Fiber.t =
  let* () = Scheduler.yield () in
  raise Exit
;;

let long_running_fiber () =
  let rec loop n =
    if n = 0
    then Fiber.return ()
    else
      let* () = Scheduler.yield () in
      loop (n - 1)
  in
  loop 10
;;

let never_fiber () = Fiber.never
let backtrace_result dyn_of_ok = Result.to_dyn dyn_of_ok (list Exn_with_backtrace.to_dyn)
let unit_result dyn_of_ok = Result.to_dyn dyn_of_ok unit

let test ?(expect_never = false) to_dyn f =
  let never_raised = ref false in
  (try Scheduler.run f |> to_dyn |> print_dyn with
   | Test_scheduler.Never -> never_raised := true);
  match !never_raised, expect_never with
  | false, false ->
    (* We don't raise in this case b/c we assume something else is being
       tested *)
    ()
  | true, true -> print_endline "[PASS] Never raised as expected"
  | false, true -> print_endline "[FAIL] expected Never to be raised but it wasn't"
  | true, false -> print_endline "[FAIL] unexpected Never raised"
;;
