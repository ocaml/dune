include Stdune
module Stringlike = Dune_util.Stringlike

module type Stringlike = Dune_util.Stringlike

include struct
  open Dune_lang
  module Package_name = Package_name
  module String_with_vars = String_with_vars
  module Section = Section
end

module Context_name = Dune_engine.Context_name
module Dpath = Dune_engine.Dpath
