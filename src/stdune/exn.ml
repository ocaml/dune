type t = exn

external raise         : exn -> _ = "%raise"
external raise_notrace : exn -> _ = "%raise_notrace"
external reraise       : exn -> _ = "%reraise"

let protectx x ~f ~finally =
  match f x with
  | y           -> finally x; y
  | exception e -> finally x; raise e

let protect ~f ~finally = protectx () ~f ~finally

include
  ((struct
    [@@@warning "-32-3"]
    let raise_with_backtrace exn _ = reraise exn
    include Printexc
    let raise_with_backtrace exn bt = raise_with_backtrace exn bt
  end) : (sig
     val raise_with_backtrace: exn -> Printexc.raw_backtrace -> _
   end))
