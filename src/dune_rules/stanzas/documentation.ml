open Import

type t =
  { loc : Loc.t
  ; package : Package.t
  ; mld_files : Ordered_set_lang.t
  }

include Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)

let decode =
  let open Dune_lang.Decoder in
  fields
    (let+ package = Stanza_common.Pkg.field ~stanza:"documentation"
     and+ mld_files = Ordered_set_lang.field "mld_files"
     and+ loc = loc in
     { loc; package; mld_files })
;;
