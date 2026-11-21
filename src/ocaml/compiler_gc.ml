let mib = 1024 * 1024
let minor_heap_size = 2 * mib
let supports_small_heap_limit (version : int * int * int) = version >= (5, 5, 0)

(* Convert the nominal 128 MiB target using the compiler's word size.
   The runtime counts GC-work units; this is not a byte limit. *)
let small_heap_limit_bytes = 128 * mib

let runparam ~word_size ~use_small_heap_limit =
  let minor_heap = Printf.sprintf "s=%dM" (minor_heap_size / mib) in
  if use_small_heap_limit
  then (
    let limit = small_heap_limit_bytes / (word_size / 8) in
    Printf.sprintf "%s,Xsmall_heap_limit=%d" minor_heap limit)
  else minor_heap
;;
