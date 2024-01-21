open Import
open Dune_lang.Decoder

module Old_name = struct
  type deprecation =
    | Not_deprecated
    | Deprecated of { deprecated_package : Package.Name.t }

  type t = Public_lib.t * deprecation

  let decode =
    let+ public = Public_lib.decode ~allow_deprecated_names:true in
    let deprecation =
      let deprecated_package = Lib_name.package_name (Public_lib.name public) in
      if let name = Package.name (Public_lib.package public) in
         Package.Name.equal deprecated_package name
      then Not_deprecated
      else Deprecated { deprecated_package }
    in
    public, deprecation
  ;;
end

type t = Old_name.t Library_redirect.t

include Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)

let old_public_name (t : t) = Public_lib.name (fst t.old_name)

let decode =
  fields
    (let+ loc = loc
     and+ project = Dune_project.get_exn ()
     and+ old_name = field "old_public_name" Old_name.decode
     and+ new_public_name = field "new_public_name" (located Lib_name.decode) in
     let () =
       let loc, old_name = (fst old_name).name in
       if Lib_name.equal (snd new_public_name) old_name
       then
         User_error.raise
           ~loc
           [ Pp.text "old_public_name cannot be the same as the new_public_name" ]
     in
     { Library_redirect.loc; project; old_name; new_public_name })
;;
