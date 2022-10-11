external stub_byte_and_native : unit -> int = "caml_42"

let byte_or_native () =
  Printf.printf "Running[]: Byte and native (42) ? %i\n"
    (stub_byte_and_native ())
