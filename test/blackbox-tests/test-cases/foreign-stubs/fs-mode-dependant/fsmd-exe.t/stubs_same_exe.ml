external stub_byte_and_native : unit -> int = "caml_b_and_n"

let () = Printf.printf "Byte (0) and native (0) ? %i\n" (stub_byte_and_native ())
