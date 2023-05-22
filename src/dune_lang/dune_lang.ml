include Dune_sexp
module Alias = Alias
module Format = Format
module Stanza = Stanza
module Glob = Glob
module String_with_vars = String_with_vars
module Pform = Pform
module Action = Action
module Dune_file_script = Dune_file_script
module Value = Value
module Blang = Blang
module Binary_kind = Binary_kind
module Package_name = Package_name
module Pkg = Pkg
module Ordered_set_lang = Ordered_set_lang
module Format_config = Format_config
module Bindings = Bindings

let decode_predicate_lang_glob : Predicate_lang.Glob.t Dune_sexp.Decoder.t =
  Predicate_lang.decode
    (Dune_sexp.Decoder.map Glob.decode ~f:Predicate_lang.Glob.create_glob)
