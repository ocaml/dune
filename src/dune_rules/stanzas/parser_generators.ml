open Import

type t =
  { loc : Loc.t
  ; modules : Ordered_set_lang.Unexpanded.t
  ; mode : Rule_mode.t
  ; enabled_if : Blang.t
  ; flags : Ordered_set_lang.Unexpanded.t
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
  let module Ast = Dune_lang.Ast in
  let short_form =
    let+ loc = loc
    and+ modules = Ordered_set_lang.Unexpanded.decode_since_expanded ~since_expanded in
    { loc
    ; modules
    ; mode = Standard
    ; enabled_if = Blang.true_
    ; flags = Ordered_set_lang.Unexpanded.standard
    }
  in
  let long_form =
    fields
      (let+ loc = loc
       and+ modules = Ordered_set_lang.Unexpanded.field ~since_expanded "modules"
       and+ mode = Rule_mode_decoder.field
       and+ enabled_if = Enabled_if.decode ~allowed_vars:Any ~since:(Some (1, 4)) ()
       and+ flags =
         Ordered_set_lang.Unexpanded.field
           ~check:(Dune_lang.Syntax.since Stanza.syntax (3, 25))
           "flags"
       in
       { loc; modules; mode; enabled_if; flags })
  in
  peek
  >>= function
  | Some (Ast.List (_, Ast.Atom (_, Dune_lang.Atom.A field_name) :: _))
    when List.mem
           [ "modules"; "mode"; "enabled_if"; "flags" ]
           field_name
           ~equal:String.equal -> long_form
  | _ -> short_form
;;
