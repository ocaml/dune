external stub_byte_or_native : unit -> int = "caml_b_or_n"

let byte_or_native () = 
  Printf.printf "Running[]: Byte (0) or native (1) ? %i\n" 
    (stub_byte_or_native ())
