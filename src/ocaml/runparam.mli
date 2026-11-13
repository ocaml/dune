(** OCaml runtime parameters and their environment-variable conventions. *)

open Stdune

(** Add a single runtime-parameter override unless [OCAMLRUNPARAM] is set in the
    inherited environment or the action overrides, including empty values. The
    inherited environment is not copied into the result.

    [minor_heap_size] is in words. [small_heap_limit] is in GC-work units, not
    bytes. *)
val add_defaults
  :  minor_heap_size:int
  -> small_heap_limit:int option
  -> inherited:Env.t
  -> overrides:Env.t
  -> Env.t
