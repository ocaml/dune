open Stdune

(** [fetch loc url ~target] will fetch [url] into [target]. If there's an error
    fetching, [loc] will be used as the location for the error. *)
val fetch : Loc.t -> OpamUrl.t -> target:Path.t -> unit Fiber.t
