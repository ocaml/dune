let bool_to_string (b : Datatypes.bool) : string =
  match b with
  | Datatypes.Coq_true -> "true"
  | Datatypes.Coq_false -> "false"

let () =
Datatypes.Coq_true
|> My_coq_file.nb
|> bool_to_string
|>  Printf.printf "Result: %s\n"