type t = int

external now : unit -> t = "dune_clock_gettime_realtime"

let start = now ()
let ns_per_sec = 1_000_000_000
let ns_per_sec_float = float_of_int ns_per_sec
let to_secs t = float_of_int t /. ns_per_sec_float
let to_ns t = t
let of_epoch_secs x = int_of_float (x *. ns_per_sec_float)
let of_ns x = x

module Span = struct
  type t = int

  let zero = 0
  let max = max
  let compare a b = Int.compare a b
  let of_secs x = int_of_float (x *. ns_per_sec_float)
  let to_secs t = float_of_int t /. ns_per_sec_float
  let of_ns x = x
  let to_ns x = x
  let add = ( + )
  let diff = ( - )
end

let add t span = t + span
let diff t1 t2 = t1 - t2
let ( > ) a b = a > b
let ( >= ) a b = a >= b
