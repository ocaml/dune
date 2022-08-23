module Iocp = struct
  type t

  external create : unit -> t = "winwatch_iocp_create"

  external run : t -> (unit, exn) result = "winwatch_iocp_run"
end

module Event = struct
  type t =
    | Added
    | Removed
    | Modified
    | Renamed_old
    | Renamed_new
end

type t

external create : string -> f:(Event.t -> string -> unit) -> t option
  = "winwatch_create"

external start : t -> Iocp.t -> unit = "winwatch_start"

external stop : t -> unit = "winwatch_stop"
