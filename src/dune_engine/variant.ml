open! Stdune
include String

let make s = s

let to_string t = t

let ppx_driver = "ppx_driver"

let mt = "mt"

let mt_posix = "mt_posix"

let byte = "byte"

let native = "native"

let plugin = "plugin"

let encode t = Dune_lang.atom_or_quoted_string t

let decode = Dune_lang.Decoder.string
