open! Stdune

(** [with_flock path ~name_for_messages ~timeout_seconds ~f] ensures
    mutual exclusion for the function [f] across multiple concurrent
    instances of dune, using the lock file at [path] to coordinate
    between different dune instances. If the lock is not acquired
    after [timeout_seconds] seconds then a [User_error] is
    raised. Pass [infinity] to keep trying to take the lock
    forever. [name_for_messages] is the name used to refer to this
    lock in error messages.

    Within the a dune process, this function also ensures mutual
    exclusion between fibers. Note that if this function times out
    waiting for the lock while the lock is held by a different fiber
    of the same dune process, a [Code_error] is raised rather than a
    [User_error]. If a timeout is possible, avoid allowing multiple
    fibers to concurrently attempt to take a flock. *)
val with_flock
  :  Path.t
  -> name_for_messages:string
  -> timeout_seconds:float
  -> f:(unit -> 'a Fiber.t)
  -> 'a Fiber.t
