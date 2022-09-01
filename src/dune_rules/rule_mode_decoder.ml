open Import
open Dune_lang.Decoder

module Promote = struct
  let into_decode =
    let+ loc, dir = located relative_file in
    { Rule.Promote.Into.loc; dir }

  let decode : Rule.Promote.t Dune_lang.Decoder.t =
    fields
      (let+ until_clean =
         field_b "until-clean"
           ~check:(Dune_lang.Syntax.since Stanza.syntax (1, 10))
       and+ into =
         field_o "into"
           (Dune_lang.Syntax.since Stanza.syntax (1, 10) >>> into_decode)
       and+ only =
         field_o "only"
           (Dune_lang.Syntax.since Stanza.syntax (1, 10)
           >>> Predicate_lang.decode Glob.decode)
       in
       let only =
         Option.map only ~f:(fun only ->
             let only = Predicate_lang.map only ~f:Glob.to_predicate in
             Predicate_lang.to_predicate only ~standard:Predicate_lang.any)
       in
       { Rule.Promote.lifetime =
           (if until_clean then Until_clean else Unlimited)
       ; into
       ; only
       })
end

let mode_decoders =
  [ ("standard", return Rule.Mode.Standard)
  ; ("fallback", return Rule.Mode.Fallback)
  ; ( "promote"
    , let+ p = Promote.decode in
      Rule.Mode.Promote p )
  ; ( "promote-until-clean"
    , let+ () =
        Dune_lang.Syntax.deleted_in Stanza.syntax (3, 0)
          ~extra_info:"Use the (promote (until-clean)) syntax instead."
      in
      Rule.Mode.Promote { lifetime = Until_clean; into = None; only = None } )
  ; ( "promote-into"
    , let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 8)
      and+ () =
        Dune_lang.Syntax.deleted_in Stanza.syntax (3, 0)
          ~extra_info:"Use the (promote (into <dir>)) syntax instead."
      and+ into = Promote.into_decode in
      Rule.Mode.Promote { lifetime = Unlimited; into = Some into; only = None }
    )
  ; ( "promote-until-clean-into"
    , let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 8)
      and+ () =
        Dune_lang.Syntax.deleted_in Stanza.syntax (3, 0)
          ~extra_info:
            "Use the (promote (until-clean) (into <dir>)) syntax instead."
      and+ into = Promote.into_decode in
      Rule.Mode.Promote
        { lifetime = Until_clean; into = Some into; only = None } )
  ]

module Extended = struct
  type t =
    | Normal of Rule.Mode.t
    | Patch_back_source_tree

  let patch_back_from_source_tree_syntax =
    Dune_lang.Syntax.create ~experimental:true ~name:"patch-back-source-tree"
      ~desc:"experimental support for (mode patch-back-source-tree)"
      [ ((0, 1), `Since (3, 0)) ]

  let () =
    Dune_project.Extension.register_simple patch_back_from_source_tree_syntax
      (Dune_lang.Decoder.return [])

  let decode =
    sum
      (( "patch-back-source-tree"
       , let+ () =
           Dune_lang.Syntax.since patch_back_from_source_tree_syntax (0, 1)
         in
         Patch_back_source_tree )
      :: List.map mode_decoders ~f:(fun (name, dec) ->
             ( name
             , let+ x = dec in
               Normal x )))

  let field = field "mode" decode ~default:(Normal Standard)
end

let decode = sum mode_decoders

let field = field "mode" decode ~default:Rule.Mode.Standard
