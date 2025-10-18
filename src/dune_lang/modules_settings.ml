open Import

type t =
  { root_module : (Loc.t * Module_name.t) option
  ; modules_without_implementation : Ordered_set_lang.Unexpanded.t
  ; modules : Ordered_set_lang.Unexpanded.t
  }

let since_expanded = 3, 13

let decode =
  let open Decoder in
  let+ root_module = field_o "root_module" Module_name.decode_loc
  and+ modules_without_implementation =
    Ordered_set_lang.Unexpanded.field ~since_expanded "modules_without_implementation"
  and+ modules = Ordered_set_lang.Unexpanded.field ~since_expanded "modules" in
  { root_module; modules; modules_without_implementation }
;;
