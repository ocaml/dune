open Stdune

let to_sexp (t : Stdlib.Gc.stat) : Sexp.t =
  let open Sexp in
  List
    [ List [ Atom "minor_words"; Atom (string_of_float t.minor_words) ]
    ; List [ Atom "promoted_words"; Atom (string_of_float t.promoted_words) ]
    ; List [ Atom "major_words"; Atom (string_of_float t.major_words) ]
    ; List
        [ Atom "minor_collections"; Atom (string_of_int t.minor_collections) ]
    ; List
        [ Atom "major_collections"; Atom (string_of_int t.major_collections) ]
    ; List [ Atom "heap_words"; Atom (string_of_int t.heap_words) ]
    ; List [ Atom "heap_chunks"; Atom (string_of_int t.heap_chunks) ]
    ; List [ Atom "live_words"; Atom (string_of_int t.live_words) ]
    ; List [ Atom "live_blocks"; Atom (string_of_int t.live_blocks) ]
    ; List [ Atom "free_words"; Atom (string_of_int t.free_words) ]
    ; List [ Atom "free_blocks"; Atom (string_of_int t.free_blocks) ]
    ; List [ Atom "largest_free"; Atom (string_of_int t.largest_free) ]
    ; List [ Atom "fragments"; Atom (string_of_int t.fragments) ]
    ; List [ Atom "compactions"; Atom (string_of_int t.compactions) ]
    ; List [ Atom "top_heap_words"; Atom (string_of_int t.top_heap_words) ]
    ; List [ Atom "stack_size"; Atom (string_of_int t.stack_size) ]
      (* forced_major_collections is only available from 4.12 so not worth it *)
    ]

let serialize (t : Stdlib.Gc.stat) ~path =
  Io.with_file_out path ~f:(fun oc ->
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%a%!" Pp.to_fmt (Sexp.pp (to_sexp t)))
