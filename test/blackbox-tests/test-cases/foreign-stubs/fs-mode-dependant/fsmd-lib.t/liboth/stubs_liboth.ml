external stub_byte_or_native : unit -> int = "caml_42"

let byte_or_native () = 
  Printf.printf "Running[]: Byte (0) or native (1) ? %i\n" 
    (stub_byte_or_native ())
