open Dune_sexp

type t =
  | System_shell
  | Custom_shell of
      { prog : String_with_vars.t
      ; args : String_with_vars.t list
      }

let default : t = System_shell

let map f = function
  | System_shell as s -> s
  | Custom_shell { prog; args } -> Custom_shell { prog = f prog; args = List.map f args }
;;

let encode : t -> Dune_sexp.t list = function
  | System_shell -> [ atom ":system" ]
  | Custom_shell { prog; args } ->
    String_with_vars.encode prog :: List.map String_with_vars.encode args
;;

let decode : t Decoder.t =
  let open Decoder in
  let kw lit v =
    let+ _ = keyword lit in
    v
  in
  kw ":system" System_shell
  <|> let+ prog = String_with_vars.decode
      and+ args = repeat String_with_vars.decode in
      Custom_shell { prog; args }
;;
