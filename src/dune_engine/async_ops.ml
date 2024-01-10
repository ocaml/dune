open Import

let maybe_async_rule_file_op f =
  (* It would be nice to do this check only once and return a function, but the
     type of this function would need to be polymorphic which is forbidden by
     the relaxed value restriction. *)
  match Config.(get background_file_system_operations_in_rule_execution) with
  | `Enabled -> Scheduler.async_exn f
  | `Disabled -> Fiber.return (f ())
;;

let maybe_async_actions : _ -> unit Fiber.t =
  let maybe_async =
    lazy
      (match Config.(get background_actions) with
       | `Enabled -> Scheduler.async_exn
       | `Disabled -> fun f -> Fiber.return (f ()))
  in
  fun f -> (Lazy.force maybe_async) f
;;

let maybe_async_sandbox f =
  (* It would be nice to do this check only once and return a function, but the
     type of this function would need to be polymorphic which is forbidden by the
     relaxed value restriction. *)
  match Config.(get background_sandboxes) with
  | `Disabled -> Fiber.return (f ())
  | `Enabled -> Scheduler.async_exn f
;;
