open! Import
open Node

type t = Exn_with_backtrace.t -> unit Fiber.t

let var : t option Fiber.Var.t = Fiber.Var.create None
let is_set = Fiber.map (Fiber.Var.get var) ~f:Option.is_some
let get_exn = Fiber.Var.get_exn var

let report_error error =
  let open Fiber.O in
  let* handler = get_exn in
  let* stack = Call_stack.get_call_stack_without_state () in
  let error =
    Exn_with_backtrace.map error ~f:(fun exn ->
      List.fold_left stack ~init:exn ~f:(fun exn stack_frame ->
        Error.extend_stack exn ~stack_frame))
  in
  Fiber.map
    (Fiber.collect_errors (fun () -> handler error))
    ~f:(function
      | Ok () -> ()
      | Error e ->
        (* Unfortunately, by re-raising an exception here we're violating some
             Memo invariants and causing more confusing exceptions, but
             hopefully this code_error will be a hint. *)
        Code_error.raise
          "Memo error handler raised an exception"
          [ "exns", Dyn.list Exn_with_backtrace.to_dyn e ])
;;

let deduplicate_errors f =
  let reported = ref Exn_set.empty in
  fun exn ->
    if Exn_set.mem !reported exn
    then Fiber.return ()
    else (
      reported := Exn_set.add !reported exn;
      f exn)
;;

let with_error_handler t f =
  Fiber.of_thunk (fun () ->
    (* [with_error_handler] runs once for every incremental run, so calling
         [deduplicate_errors] afresh here makes sure that we re-report all
         errors*)
    let t = deduplicate_errors t in
    Fiber.bind (Fiber.Var.get var) ~f:(function
      | None -> Fiber.Var.set var (Some t) f
      | Some _handler ->
        Code_error.raise
          "Memo.run_with_error_handler: an error handler is already installed"
          []))
;;
