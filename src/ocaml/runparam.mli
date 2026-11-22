(** OCaml runtime parameters and their environment-variable conventions. *)

open Stdune

(** Add a single runtime-parameter override unless [OCAMLRUNPARAM] is set in the
    inherited environment or the action overrides, including empty values. The
    inherited environment is not copied into the result. [word_size] is in bits. *)
val add_defaults
  :  word_size:int
  -> use_small_heap_limit:bool
  -> inherited:Env.t
  -> overrides:Env.t
  -> Env.t
