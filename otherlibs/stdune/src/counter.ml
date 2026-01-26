module type S = sig
  type t
  type u

  val create : unit -> t
  val read : t -> u
  val add : t -> u -> unit
  val reset : t -> unit
end

type t = int ref

let create () = ref 0
let read t = !t
let incr t = incr t
let add t count = t := !t + count
let reset t = t := 0

module Timer = struct
  type t = Time.Span.t ref

  let create () = ref Time.Span.zero
  let read t = !t
  let add t span = t := Time.Span.add !t span
  let reset t = t := Time.Span.zero

  type start = Time.t

  let start () = Time.now ()
  let stop t start = add t (Time.diff (Time.now ()) start)
end
