(** Combinators to optionally run computations asynchronously.

    Depending on a particular configuration value, these will run the argument
    function either synchronously or in the background. *)

(** Perform a function asynchronously or synchronously depending on
    [Config.background_file_system_operations_in_rule_execution]. *)
val maybe_async_rule_file_op : (unit -> 'a) -> 'a Fiber.t

(** Perform a function asynchronously or synchronously depending on
    [Config.background_actions]. *)
val maybe_async_actions : (unit -> 'a) -> 'a Fiber.t

(** Perform a function asynchronously or synchronously depending on
    [Config.background_sandboxes]. *)
val maybe_async_sandbox : (unit -> 'a) -> 'a Fiber.t
