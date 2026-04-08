open Import

type t =
  | No
  | Yes of
      { transition : string option
      ; module_name : Module_name.t option
      }

let equal = Poly.equal

let decode =
  let open Decoder in
  peek_exn
  >>= function
  | Atom (_, A "true") -> junk >>> return (Yes { transition = None; module_name = None })
  | Atom (_, A "false") -> junk >>> return No
  | List _ ->
    fields
      (let+ transition =
         field_o "transition" (Syntax.since Stanza.syntax (1, 2) >>> string)
       and+ module_name =
         field_o "module_name" (Syntax.since Stanza.syntax (3, 18) >>> Module_name.decode)
       in
       Yes { transition; module_name })
  | sexp ->
    User_error.raise
      ~loc:(Ast.loc sexp)
      [ Pp.text "expected true, false, or a list of wrapped options" ]
;;

let encode =
  let open Encoder in
  function
  | No -> bool false
  | Yes { transition = None; module_name = None } -> bool true
  | Yes { transition; module_name } ->
    record
      (List.filter_map
         ~f:Fun.id
         [ Option.map transition ~f:(fun t -> "transition", string t)
         ; Option.map module_name ~f:(fun m -> "module_name", Module_name.encode m)
         ])
;;

let to_bool = function
  | No -> false
  | Yes _ -> true
;;

let module_name = function
  | Yes { module_name; _ } -> module_name
  | No -> None
;;

let transition = function
  | Yes { transition; _ } -> transition
  | No -> None
;;

let to_dyn =
  let open Dyn in
  function
  | No -> variant "No" []
  | Yes { transition; module_name } ->
    variant
      "Yes"
      [ record
          [ "transition", option string transition
          ; "module_name", option Module_name.to_dyn module_name
          ]
      ]
;;
