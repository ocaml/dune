(** GC defaults shared by Dune's compiler rules and its bootstrap script.
    This module must only depend on the standard library. *)

val supports_small_heap_limit : int * int * int -> bool

(** Encode the GC defaults for [OCAMLRUNPARAM]. [word_size] is in bits. *)
val runparam : word_size:int -> use_small_heap_limit:bool -> string
