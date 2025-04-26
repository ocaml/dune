external baz : unit -> int = "baz"

external hello_world_baz : unit -> unit = "hello_world_baz"

let quad x = baz x
let hello () = hello_world_baz ()
