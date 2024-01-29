open Import
open Dune_lang.Decoder

type origin =
  | Build
  | Source

type t =
  { add_line_directive : bool
  ; alias : Alias.Name.t option
  ; mode : Rule.Mode.t
  ; enabled_if : Blang.t
  ; files : String_with_vars.t
  ; origin : origin
  ; syntax_version : Dune_lang.Syntax.Version.t
  }

include Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)

let long_form =
  let check = Dune_lang.Syntax.since Stanza.syntax (2, 7) in
  let+ alias = field_o "alias" (check >>> Dune_lang.Alias.decode)
  and+ mode = field "mode" ~default:Rule.Mode.Standard (check >>> Rule_mode_decoder.decode)
  and+ enabled_if = Enabled_if.decode ~allowed_vars:Any ~since:(Some (2, 8)) ()
  and+ files = field "files" (check >>> String_with_vars.decode)
  and+ only_sources = field_b "only_sources" ~check:(Dune_lang.Syntax.since Stanza.syntax (3, 14))
  and+ syntax_version = Dune_lang.Syntax.get_exn Stanza.syntax in
  let origin =
    match only_sources with
    | true -> Source
    | false -> Build
  in
  { add_line_directive = false; alias; mode; enabled_if; files; origin; syntax_version }
;;

let decode =
  peek_exn
  >>= function
  | List _ -> fields long_form
  | _ ->
    let+ files = String_with_vars.decode
    and+ syntax_version = Dune_lang.Syntax.get_exn Stanza.syntax in
    { add_line_directive = false
    ; alias = None
    ; mode = Standard
    ; enabled_if = Blang.true_
    ; files
    ; origin = Build
    ; syntax_version
    }
;;
