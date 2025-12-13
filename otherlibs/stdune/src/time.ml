type t = float

let now () = Unix.gettimeofday ()
let to_secs t = t
let of_epoch_secs x = x
let diff = ( -. )

module Span = struct
  type t = float

  let zero = 0.
  let max = Float.max
  let compare = Float.compare
  let of_secs x = x
  let add = ( +. )
  let diff = ( -. )
  let to_secs t = t
end

let add t x = t +. x
