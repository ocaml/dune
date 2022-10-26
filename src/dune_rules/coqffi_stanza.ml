open Import
open Dune_lang.Decoder

let syntax =
  Dune_lang.Syntax.create ~name:"coqffi" ~desc:"Support for coqffi"
    [ ((0, 1), `Since (3, 7)) ]

type t =
  { modules : Module_name.t list
  ; loc : Loc.t
  ; library : Loc.t * Lib_name.t
  ; flags : Ordered_set_lang.t
  }

let decode =
  fields
    (let+ modules = field "modules" (repeat Module_name.decode)
     and+ loc = loc
     and+ library = field "library" (located Lib_name.decode)
     and+ flags = Ordered_set_lang.field "flags" in
     { modules; loc; library; flags })

type Stanza.t += T of t

let () =
  Dune_project.Extension.register_simple syntax
    (return [ ("coqffi", decode >>| fun x -> [ T x ]) ])
