type t = exn

external raise         : exn -> _ = "%raise"
external raise_notrace : exn -> _ = "%raise_notrace"
external reraise       : exn -> _ = "%reraise"

let protectx x ~f ~finally =
  match f x with
  | y           -> finally x; y
  | exception e -> finally x; raise e

let protect ~f ~finally = protectx () ~f ~finally
