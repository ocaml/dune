external prim : unit -> bool = "wasmoo_stubs"

let _ = assert (Sys.int_size = 31)
let _ = assert (prim ())
let _ = print_endline "inline tests (Wasm)"
