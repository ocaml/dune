open Import

let syntax =
  Dune_lang.Syntax.create ~name:"menhir" ~desc:"the menhir extension"
    [ ((1, 0), `Since (1, 0))
    ; ((1, 1), `Since (1, 4))
    ; ((2, 0), `Since (1, 4))
    ; ((2, 1), `Since (2, 2))
    ]

open Dune_lang.Decoder

type t =
  { merge_into : string option
  ; flags : Ordered_set_lang.Unexpanded.t
  ; modules : string list
  ; mode : Rule.Mode.t
  ; loc : Loc.t
  ; infer : bool
  ; enabled_if : Blang.t
  }

let decode =
  fields
    (let+ merge_into = field_o "merge_into" string
     and+ flags = Ordered_set_lang.Unexpanded.field "flags"
     and+ modules = field "modules" (repeat string)
     and+ mode = Rule_mode_decoder.field
     and+ infer =
       field_o_b "infer" ~check:(Dune_lang.Syntax.since syntax (2, 0))
     and+ menhir_syntax = Dune_lang.Syntax.get_exn syntax
     and+ enabled_if =
       Enabled_if.decode ~allowed_vars:Any ~since:(Some (1, 4)) ()
     and+ loc = loc in
     let infer =
       match infer with
       | Some infer -> infer
       | None -> menhir_syntax >= (2, 0)
     in
     { merge_into; flags; modules; mode; loc; infer; enabled_if })

type Stanza.t += T of t

let () =
  Dune_project.Extension.register_simple syntax
    (return [ ("menhir", decode >>| fun x -> [ T x ]) ])

let modules (stanza : t) : string list =
  match stanza.merge_into with
  | Some m -> [ m ]
  | None -> stanza.modules

let targets (stanza : t) : string list =
  let f m = [ m ^ ".ml"; m ^ ".mli" ] in
  List.concat_map (modules stanza) ~f
