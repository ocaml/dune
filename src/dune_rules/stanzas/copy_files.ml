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
  ; only_sources : Blang.t
  ; syntax_version : Dune_lang.Syntax.Version.t
  }

include Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)

let decode_only_sources =
  let* blang = peek in
  match blang with
  | Some _ -> Blang.decode
  | None -> return Blang.true_
;;

let long_form =
  let check = Dune_lang.Syntax.since Stanza.syntax (2, 7) in
  let+ alias = field_o "alias" (check >>> Dune_lang.Alias.decode)
  and+ mode = field "mode" ~default:Rule.Mode.Standard (check >>> Rule_mode_decoder.decode)
  and+ enabled_if = Enabled_if.decode ~allowed_vars:Any ~since:(Some (2, 8)) ()
  and+ files = field "files" (check >>> String_with_vars.decode)
  and+ only_sources =
    field_o
      "only_sources"
      (Dune_lang.Syntax.since Stanza.syntax (3, 14) >>> decode_only_sources)
  and+ syntax_version = Dune_lang.Syntax.get_exn Stanza.syntax in
  let only_sources = Option.value only_sources ~default:Blang.false_ in
  { add_line_directive = false
  ; alias
  ; mode
  ; enabled_if
  ; files
  ; only_sources
  ; syntax_version
  }
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
    ; only_sources = Blang.false_
    ; syntax_version
    }
;;
