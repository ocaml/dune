open Stdune
open Dune_tests_common

(* Shared harness for the Memo unit tests, factored out so the focused test files
   below can each [open Test_helpers.Make ()] for a fresh scheduler. *)
module Make () = struct
  module Scheduler = struct
    let t = Test_scheduler.create ()
    let yield () = Test_scheduler.yield t
    let run f = Test_scheduler.run t f
  end

  let () = init ()
  let printf = Printf.printf
  let printfn fmt = Printf.ksprintf (fun s -> printf "%s\n" s) fmt
  let () = Memo.Debug.check_invariants := true

  let print_metrics () =
    Memo.Metrics.assert_invariants ();
    printf "%s\n" (Memo.Metrics.report ~reset_after_reporting:true)
  ;;

  let string_fn_create name = Memo.create name ~input:(module String) ~cutoff:String.equal
  let int_fn_create name ~cutoff = Memo.create name ~input:(module Int) ~cutoff
  let run m = Scheduler.run (Memo.run m)

  let run_memo f v =
    try run (Memo.exec f v) with
    | Memo.Error.E err -> raise (Memo.Error.get err)
  ;;

  let print_cycle_error cycle_error =
    let frames = Memo.Cycle_error.get cycle_error in
    printf "Dependency cycle detected:\n";
    List.iteri frames ~f:(fun i frame ->
      let called_by =
        match i with
        | 0 -> ""
        | _ -> "called by "
      in
      printf "- %s%s\n" called_by (Dyn.to_string (Memo.Stack_frame.to_dyn frame)))
  ;;

  let print_result arg res =
    let res =
      Result.map_error
        res
        ~f:
          (List.map
             ~f:
               (Exn_with_backtrace.map ~f:(fun exn ->
                  match exn with
                  | Memo.Cycle_error.E error ->
                    print_cycle_error error;
                    exn
                  | _ -> exn)))
    in
    let open Dyn in
    Format.printf
      "f %d = %a@."
      arg
      Pp.to_fmt
      (Dyn.pp (Result.to_dyn int (list Exn_with_backtrace.to_dyn) res))
  ;;

  let run_collect_errors f =
    let open Fiber.O in
    Fiber.collect_errors (fun () -> Memo.run (f ()))
    >>| function
    | Ok _ as res -> res
    | Error errs ->
      Error
        (List.map errs ~f:(fun (e : Exn_with_backtrace.t) ->
           match e.exn with
           | Memo.Error.E err -> { e with exn = Memo.Error.get err }
           | _ -> e))
  ;;

  let evaluate_and_print f x =
    let res =
      try
        Fiber.run
          ~iter:(fun () -> raise Exit)
          (run_collect_errors (fun () -> Memo.exec f x))
      with
      | exn -> Error [ Exn_with_backtrace.capture exn ]
    in
    print_result x res
  ;;
end
