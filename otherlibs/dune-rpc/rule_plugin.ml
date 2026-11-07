open Import
module Wire = Csexp

module Sexp = struct
  type pform =
    { name : string
    ; payload : string option
    }

  type template_part =
    | Text of string
    | Pform of pform

  type template =
    { quoted : bool
    ; parts : template_part list
    }

  type t =
    | Atom of string
    | Quoted_string of string
    | Template of template
    | List of t list
end

let invalid wire =
  Conv.error
    (Parse_error
       { message = "invalid s-expression in rule plugin protocol"
       ; payload = [ "sexp", wire ]
       })
;;

let option_to_wire = function
  | None -> Wire.List []
  | Some value -> List [ Atom value ]
;;

let option_of_wire = function
  | Wire.List [] -> None
  | List [ Atom value ] -> Some value
  | wire -> invalid wire
;;

let template_part_to_wire = function
  | Sexp.Text text -> Wire.List [ Atom "text"; Atom text ]
  | Pform { name; payload } -> List [ Atom "pform"; Atom name; option_to_wire payload ]
;;

let template_part_of_wire = function
  | Wire.List [ Atom "text"; Atom text ] -> Sexp.Text text
  | List [ Atom "pform"; Atom name; payload ] ->
    Pform { Sexp.name; payload = option_of_wire payload }
  | wire -> invalid wire
;;

let rec to_wire = function
  | Sexp.Atom atom -> Wire.List [ Atom "atom"; Atom atom ]
  | Quoted_string string -> List [ Atom "quoted"; Atom string ]
  | Template { quoted; parts } ->
    List
      [ Atom "template"
      ; Atom (Bool.to_string quoted)
      ; List (List.map parts ~f:template_part_to_wire)
      ]
  | List sexps -> List (Atom "list" :: List.map sexps ~f:to_wire)
;;

let rec of_wire = function
  | Wire.List [ Atom "atom"; Atom atom ] -> Sexp.Atom atom
  | List [ Atom "quoted"; Atom string ] -> Quoted_string string
  | List [ Atom "template"; Atom quoted; List parts ] as wire ->
    let quoted =
      match bool_of_string_opt quoted with
      | Some quoted -> quoted
      | None -> invalid wire
    in
    Template { Sexp.quoted; parts = List.map parts ~f:template_part_of_wire }
  | List (Atom "list" :: sexps) -> List (List.map sexps ~f:of_wire)
  | wire -> invalid wire
;;

let sexp = Conv.iso Conv.sexp of_wire to_wire

let make_request method_ conv =
  let generation = Types.Decl.Request.make_current_gen ~req:conv ~resp:conv ~version:1 in
  Types.Decl.Request.make
    ~method_:(Types.Method.Name.of_string method_)
    ~generations:[ generation ]
;;

let load_rules = make_request "dune/rules-plugin/load-rules" (Conv.list sexp)
let ready = make_request "dune/rules-plugin/ready" Conv.unit
