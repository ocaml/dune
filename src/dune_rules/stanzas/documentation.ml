open Import

type t =
  { loc : Loc.t
  ; package : Package.t
  ; mld_files : Ordered_set_lang.t
  ; files : Install_entry.File.t list
  }

include Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)

let decode =
  let open Dune_lang.Decoder in
  fields
    (let+ package = Stanza_pkg.field ~stanza:"documentation"
     and+ mld_files = Ordered_set_lang.field "mld_files"
     and+ files = field_o "files" (repeat Install_entry.File.decode)
     and+ loc = loc in
     let files = Option.value files ~default:[] in
     { loc; package; mld_files; files })
;;
