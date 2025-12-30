open Import

type t =
  { loc : Loc.t
  ; modules : Ordered_set_lang.Unexpanded.t
  ; mode : Rule_mode.t
  ; enabled_if : Blang.t
  }

type for_ =
  | Ocamllex of t
  | Ocamlyacc of t

let tool = function
  | Ocamllex _ -> "ocamllex"
  | Ocamlyacc _ -> "ocamlyacc"
;;

module Ocamllex = Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)

module Ocamlyacc = Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)

let since_expanded = 3, 22

let decode =
  let open Dune_lang.Decoder in
  (let+ loc = loc
   and+ modules = Ordered_set_lang.Unexpanded.decode_since_expanded ~since_expanded in
   { loc; modules; mode = Standard; enabled_if = Blang.true_ })
  <|> fields
        (let+ loc = loc
         and+ modules = Ordered_set_lang.Unexpanded.field ~since_expanded "modules"
         and+ mode = Rule_mode_decoder.field
         and+ enabled_if = Enabled_if.decode ~allowed_vars:Any ~since:(Some (1, 4)) () in
         { loc; modules; mode; enabled_if })
;;

let modules_settings modules =
  { Modules_settings.root_module = None
  ; modules_without_implementation = Ordered_set_lang.Unexpanded.standard
  ; modules
  }
;;
