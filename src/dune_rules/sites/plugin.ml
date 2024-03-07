open Import
open Dune_lang.Decoder

type t =
  { package : Package.t
  ; name : Package.Name.t
  ; libraries : (Loc.t * Lib_name.t) list
  ; site : Loc.t * (Package.Name.t * Site.t)
  ; optional : bool
  }

let decode =
  let* () = Dune_lang.Syntax.since Site.dune_site_syntax (0, 1) in
  fields
    (let+ name = field "name" Package.Name.decode
     and+ libraries = field "libraries" (repeat (located Lib_name.decode))
     and+ site = field "site" (located (pair Package.Name.decode Site.decode))
     and+ package = Stanza_common.Pkg.field ~stanza:"plugin"
     and+ optional = field_b "optional" in
     { name; libraries; site; package; optional })
;;

include Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)
