open Import

type t =
  { section : Loc.t * Section_with_site.t
  ; files : Install_entry.File.t list
  ; dirs : Install_entry.Dir.t list
  ; source_trees : Install_entry.Dir.t list
  ; package : Package.t
  ; enabled_if : Blang.t
  }

include Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)

let decode =
  let open Dune_lang.Decoder in
  fields
    (let+ loc = loc
     and+ section = field "section" (located Section_with_site.decode)
     and+ files = field_o "files" (repeat Install_entry.File.decode)
     and+ dirs =
       field_o
         "dirs"
         (Dune_lang.Syntax.since Stanza.syntax (3, 5) >>> repeat Install_entry.Dir.decode)
     and+ source_trees =
       field_o
         "source_trees"
         (Dune_lang.Syntax.since Stanza.syntax (3, 11) >>> repeat Install_entry.Dir.decode)
     and+ package = Stanza_common.Pkg.field ~stanza:"install"
     and+ enabled_if =
       let allowed_vars = Enabled_if.common_vars ~since:(2, 6) in
       Enabled_if.decode ~allowed_vars ~since:(Some (2, 6)) ()
     in
     let files, dirs, source_trees =
       match files, dirs, source_trees with
       | None, None, None ->
         User_error.raise ~loc [ Pp.textf "dirs, files, or source_trees must be set" ]
       | _, _, _ ->
         ( Option.value files ~default:[]
         , Option.value dirs ~default:[]
         , Option.value source_trees ~default:[] )
     in
     { section; dirs; files; source_trees; package; enabled_if })
;;
