include String

let empty = ""
let of_string = copy
let to_string = copy

let sub_string = sub

(* these are trivially safe and could be made into an identity
   function, but given that OCaml signatures distinguish between "val"
   and "external", I thought it best to reuse upstream's choice to
   avoid any potential copmatibility hurdles (not that I know
   of any) *)
external unsafe_to_string : t -> string = "%identity"
external unsafe_of_string : string -> t = "%identity"
