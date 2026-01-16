open Import

let syntax = Dune_lang.Menhir.syntax

open Dune_lang.Decoder

type t =
  { merge_into : string option
  ; flags : Ordered_set_lang.Unexpanded.t
  ; modules : Ordered_set_lang.Unexpanded.t
  ; mode : Rule_mode.t
  ; loc : Loc.t
  ; infer : bool
  ; enabled_if : Blang.t
  ; explain : Blang.t option
  ; menhir_syntax : Syntax.Version.t
  }

let decode =
  fields
    (let+ merge_into = field_o "merge_into" string
     and+ flags = Ordered_set_lang.Unexpanded.field "flags"
     and+ modules =
       Ordered_set_lang.Unexpanded.field
         "modules"
         ~since_expanded:Parser_generators.since_expanded
     and+ mode = Rule_mode_decoder.field
     and+ infer = field_o_b "infer" ~check:(Dune_lang.Syntax.since syntax (2, 0))
     and+ menhir_syntax = Dune_lang.Syntax.get_exn syntax
     and+ enabled_if = Enabled_if.decode ~allowed_vars:Any ~since:(Some (1, 4)) ()
     and+ loc = loc
     and+ explain =
       field_o
         "explain"
         (Dune_lang.Syntax.since syntax Dune_lang.Menhir.explain_since >>> Blang.decode)
     in
     let infer =
       match infer with
       | Some infer -> infer
       | None -> menhir_syntax >= (2, 0)
     in
     { merge_into; flags; modules; mode; loc; infer; enabled_if; explain; menhir_syntax })
;;

include Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)

let () =
  Dune_project.Extension.register_simple
    syntax
    (return [ "menhir", decode_stanza decode ])
;;
