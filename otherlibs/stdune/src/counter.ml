type t = int ref

let create () = ref 0
let read t = !t
let incr t = incr t
let add t count = t := !t + count
let reset t = t := 0
