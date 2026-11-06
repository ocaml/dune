open Stdune

let ocamlrunparam = Env.Var.of_string "OCAMLRUNPARAM"
let mb = 1024 * 1024

let encode ~minor_heap_size ~small_heap_limit =
  let size =
    if minor_heap_size mod mb = 0
    then sprintf "%dM" (minor_heap_size / mb)
    else Int.to_string minor_heap_size
  in
  let minor_heap_size = "s=" ^ size in
  match small_heap_limit with
  | None -> minor_heap_size
  | Some limit -> sprintf "%s,Xsmall_heap_limit=%d" minor_heap_size limit
;;

let add_defaults ~minor_heap_size ~small_heap_limit ~inherited ~overrides =
  if Env.mem inherited ~var:ocamlrunparam || Env.mem overrides ~var:ocamlrunparam
  then overrides
  else
    Env.add
      overrides
      ~var:ocamlrunparam
      ~value:(encode ~minor_heap_size ~small_heap_limit)
;;
