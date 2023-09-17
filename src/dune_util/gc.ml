open Stdune

let to_sexp
  ({ Stdlib.Gc.minor_words
   ; promoted_words
   ; major_words
   ; minor_collections
   ; major_collections
   ; heap_words
   ; heap_chunks
   ; live_words
   ; live_blocks
   ; free_words
   ; free_blocks
   ; largest_free
   ; fragments
   ; compactions
   ; top_heap_words
   ; stack_size
   ; _
   } :
    Stdlib.Gc.stat)
  : Sexp.t
  =
  let open Sexp in
  List
    [ List [ Atom "minor_words"; Atom (string_of_float minor_words) ]
    ; List [ Atom "promoted_words"; Atom (string_of_float promoted_words) ]
    ; List [ Atom "major_words"; Atom (string_of_float major_words) ]
    ; List [ Atom "minor_collections"; Atom (string_of_int minor_collections) ]
    ; List [ Atom "major_collections"; Atom (string_of_int major_collections) ]
    ; List [ Atom "heap_words"; Atom (string_of_int heap_words) ]
    ; List [ Atom "heap_chunks"; Atom (string_of_int heap_chunks) ]
    ; List [ Atom "live_words"; Atom (string_of_int live_words) ]
    ; List [ Atom "live_blocks"; Atom (string_of_int live_blocks) ]
    ; List [ Atom "free_words"; Atom (string_of_int free_words) ]
    ; List [ Atom "free_blocks"; Atom (string_of_int free_blocks) ]
    ; List [ Atom "largest_free"; Atom (string_of_int largest_free) ]
    ; List [ Atom "fragments"; Atom (string_of_int fragments) ]
    ; List [ Atom "compactions"; Atom (string_of_int compactions) ]
    ; List [ Atom "top_heap_words"; Atom (string_of_int top_heap_words) ]
    ; List [ Atom "stack_size"; Atom (string_of_int stack_size) ]
      (* forced_major_collections is only available from 4.12 so not worth it *)
    ]
;;

let serialize (t : Stdlib.Gc.stat) ~path =
  Io.with_file_out path ~f:(fun oc ->
    let fmt = Format.formatter_of_out_channel oc in
    Format.fprintf fmt "%a%!" Pp.to_fmt (Sexp.pp (to_sexp t)))
;;

let decode =
  (* In order to stay version independent, we use a trick with `with` by
     creating a dummy value and filling in the fields available in every OCaml
     version. forced_major_collections is the one missing in versions older than
     4.12. *)
  let dummy = Stdlib.Gc.quick_stat () in
  let open Dune_sexp.Decoder in
  enter
  @@ fields
  @@ let+ minor_words = field "minor_words" float
     and+ promoted_words = field "promoted_words" float
     and+ major_words = field "major_words" float
     and+ minor_collections = field "minor_collections" int
     and+ major_collections = field "major_collections" int
     and+ heap_words = field "heap_words" int
     and+ heap_chunks = field "heap_chunks" int
     and+ live_words = field "live_words" int
     and+ live_blocks = field "live_blocks" int
     and+ free_words = field "free_words" int
     and+ free_blocks = field "free_blocks" int
     and+ largest_free = field "largest_free" int
     and+ fragments = field "fragments" int
     and+ compactions = field "compactions" int
     and+ top_heap_words = field "top_heap_words" int
     and+ stack_size = field "stack_size" int in
     { dummy with
       Stdlib.Gc.minor_words
     ; promoted_words
     ; major_words
     ; minor_collections
     ; major_collections
     ; heap_words
     ; heap_chunks
     ; live_words
     ; live_blocks
     ; free_words
     ; free_blocks
     ; largest_free
     ; fragments
     ; compactions
     ; top_heap_words
     ; stack_size
     }
;;
