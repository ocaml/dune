external stat' : string -> float option = "dune_creation_time__stat"
external lstat' : string -> float option = "dune_creation_time__lstat"

let gen_stat f path =
  let v = f path in
  match v with
  (* use ctime on Windows? *)
  | None -> None
  | Some x -> Some x
;;

let stat = gen_stat stat'
let lstat = gen_stat lstat'
