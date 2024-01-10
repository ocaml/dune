open Import

type maybe_async_function = { apply : 'a. (unit -> 'a) -> 'a Fiber.t }

let of_config_value = function
  | `Enabled -> { apply = Scheduler.async_exn }
  | `Disabled -> { apply = (fun f -> Fiber.return (f ())) }
;;

let of_key k =
  let l = lazy (of_config_value (Config.get k)) in
  let apply f = (Lazy.force l).apply f in
  { apply }
;;

let { apply = maybe_async_rule_file_op } =
  of_key Config.background_file_system_operations_in_rule_execution
;;

let { apply = maybe_async_actions } = of_key Config.background_actions
let { apply = maybe_async_sandbox } = of_key Config.background_sandboxes
