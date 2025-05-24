open Import
open Decoder
module Rule = Dune_engine.Rule

module Promote = struct
  let into_decode =
    let+ loc, dir = located relative_file in
    { Rule.Promote.Into.loc; dir }
  ;;

  let decode : Rule.Promote.t Decoder.t =
    fields
      (let+ until_clean =
         field_b "until-clean" ~check:(Syntax.since Stanza.syntax (1, 10))
       and+ into = field_o "into" (Syntax.since Stanza.syntax (1, 10) >>> into_decode)
       and+ only =
         field_o "only" (Syntax.since Stanza.syntax (1, 10) >>> Predicate_lang.Glob.decode)
       in
       let only =
         Option.map only ~f:(fun only ->
           Predicate.create (Predicate_lang.Glob.test only ~standard:Predicate_lang.true_))
       in
       { Rule.Promote.lifetime = (if until_clean then Until_clean else Unlimited)
       ; into
       ; only
       })
  ;;
end

let mode_decoders =
  [ "standard", return Rule.Mode.Standard
  ; "fallback", return Rule.Mode.Fallback
  ; ( "promote"
    , let+ p = Promote.decode in
      Rule.Mode.Promote p )
  ; ( "promote-until-clean"
    , let+ () =
        Syntax.deleted_in
          Stanza.syntax
          (3, 0)
          ~extra_info:"Use the (promote (until-clean)) syntax instead."
      in
      Rule.Mode.Promote { lifetime = Until_clean; into = None; only = None } )
  ; ( "promote-into"
    , let+ () = Syntax.since Stanza.syntax (1, 8)
      and+ () =
        Syntax.deleted_in
          Stanza.syntax
          (3, 0)
          ~extra_info:"Use the (promote (into <dir>)) syntax instead."
      and+ into = Promote.into_decode in
      Rule.Mode.Promote { lifetime = Unlimited; into = Some into; only = None } )
  ; ( "promote-until-clean-into"
    , let+ () = Syntax.since Stanza.syntax (1, 8)
      and+ () =
        Syntax.deleted_in
          Stanza.syntax
          (3, 0)
          ~extra_info:"Use the (promote (until-clean) (into <dir>)) syntax instead."
      and+ into = Promote.into_decode in
      Rule.Mode.Promote { lifetime = Until_clean; into = Some into; only = None } )
  ]
;;

let decode = sum mode_decoders
let field = field "mode" decode ~default:Rule.Mode.Standard
