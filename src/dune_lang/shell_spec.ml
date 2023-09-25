open Dune_sexp

type t =
  | System_shell
  | Bash_shell
  | Exec_file_shell of String_with_vars.t

let default : t = System_shell

let map f = function
  | (System_shell | Bash_shell) as s -> s
  | Exec_file_shell p -> Exec_file_shell (f p)
;;

let encode : t Encoder.t = function
  | System_shell -> atom ":system"
  | Bash_shell -> atom ":bash"
  | Exec_file_shell p -> String_with_vars.encode p
;;

let decode : t Decoder.t =
  let open Decoder in
  let kw lit v =
    let+ _ = keyword lit in
    v
  in
  kw ":system" System_shell
  <|> kw ":bash" Bash_shell
  <|> let+ p = String_with_vars.decode in
      Exec_file_shell p
;;
