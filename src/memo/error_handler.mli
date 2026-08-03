open! Import

val is_set : bool Fiber.t
val report_error : Exn_with_backtrace.t -> unit Fiber.t

val with_error_handler
  :  (Exn_with_backtrace.t -> unit Fiber.t)
  -> (unit -> 'a Fiber.t)
  -> 'a Fiber.t
