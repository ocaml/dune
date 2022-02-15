
external prim : unit -> bool = "jsoo_stubs"

let _ = assert (Sys.int_size = 32)
let _ = assert (prim ())
let _ = print_endline "inline tests (JS)"
