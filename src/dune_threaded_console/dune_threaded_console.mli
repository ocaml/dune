include module type of Dune_threaded_console_intf

(** [make ~frames_per_second (module T)] is a backend that renders the user interface in a
    separate thread. The module [T] must implement the [Threaded] interface. There are
    special functions included to handle various functions of a user interface.

    The [frames_per_second] argument controls how often the user interface is updated. *)
val make : frames_per_second:int -> (module S) -> Dune_console.Backend.t

(** Threaded variant of [Dune_console.Backend.progress]. *)
val progress : frames_per_second:int -> Dune_console.Backend.t
