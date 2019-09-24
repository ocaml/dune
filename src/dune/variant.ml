open! Stdune

include Interned.Make
          (struct
            let initial_size = 256

            let resize_policy = Interned.Conservative

            let order = Interned.Fast
          end)
          ()

let ppx_driver = make "ppx_driver"

let mt = make "mt"

let mt_posix = make "mt_posix"

let byte = make "byte"

let native = make "native"

let plugin = make "plugin"

let encode t = Dune_lang.atom_or_quoted_string (to_string t)

let decode = Dune_lang.Decoder.plain_string (fun ~loc:_ s -> make s)

let syntax =
  let syntax =
    Dune_lang.Syntax.create ~name:"library_variants"
      ~desc:"the experimental library variants feature." [ (0, 2) ]
  in
  Dune_project.Extension.register_simple ~experimental:true syntax
    (Dune_lang.Decoder.return []);
  syntax
