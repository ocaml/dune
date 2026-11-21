open Stdune

let ocamlrunparam = Env.Var.of_string "OCAMLRUNPARAM"

let add_defaults ~word_size ~use_small_heap_limit ~inherited ~overrides =
  if Env.mem inherited ~var:ocamlrunparam || Env.mem overrides ~var:ocamlrunparam
  then overrides
  else
    Env.add
      overrides
      ~var:ocamlrunparam
      ~value:(Compiler_gc.runparam ~word_size ~use_small_heap_limit)
;;
