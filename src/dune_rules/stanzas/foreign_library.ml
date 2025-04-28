open Import

type t =
  { archive_name : Foreign.Archive.Name.t
  ; archive_name_loc : Loc.t
  ; stubs : Foreign.Stubs.t
  ; enabled_if : Blang.t
  ; extra_objects : Ordered_set_lang.Unexpanded.t
  }

let decode =
  let open Dune_lang.Decoder in
  fields
    (let+ archive_name_loc, archive_name =
       located (field "archive_name" Foreign.Archive.Name.decode)
     and+ stubs = Foreign.Stubs.decode_stubs ~for_library:true
     and+ enabled_if = Enabled_if.decode ~allowed_vars:Any ~since:(Some (3, 14)) ()
     and+ extra_objects =
       Ordered_set_lang.Unexpanded.field
         "extra_objects"
         ~check:(Dune_lang.Syntax.since Stanza.syntax (3, 19))
     in
     { archive_name; archive_name_loc; stubs; enabled_if; extra_objects })
;;

include Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)
