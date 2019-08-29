(* CR-someday diml: we should define a GADT for this:

   {[ type 'a t = | Int : int t | Box : ... | Colored : ... ]}

   This way we could separate the creation of messages from the actual
   rendering. *)
type 'a t = Format.formatter -> 'a -> unit

let list = Format.pp_print_list

let text = Format.pp_print_text

let nl = Format.pp_print_newline

let ocaml_list pp fmt = function
  | [] -> Format.pp_print_string fmt "[]"
  | l ->
    Format.fprintf fmt "@[<hv>[ %a@ ]@]"
      (list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,; ") pp)
      l
