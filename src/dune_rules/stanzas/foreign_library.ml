open Import

type t =
  { archive_name : Foreign.Archive.Name.t
  ; archive_name_loc : Loc.t
  ; stubs : Foreign.Stubs.t
  ; enabled_if : Blang.t
  }

let decode =
  let open Dune_lang.Decoder in
  fields
    (let+ archive_name_loc, archive_name =
       located (field "archive_name" Foreign.Archive.Name.decode)
     and+ stubs = Foreign.Stubs.decode_stubs ~for_library:true
     and+ enabled_if = Enabled_if.decode ~allowed_vars:Any ~since:(Some (3, 14)) () in
     { archive_name; archive_name_loc; stubs; enabled_if })
;;

include Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)
