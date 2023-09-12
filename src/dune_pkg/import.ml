include Stdune
module Stringlike = Dune_util.Stringlike
module Re = Dune_re

module type Stringlike = Dune_util.Stringlike

include struct
  open Dune_sexp
  module Decoder = Decoder
  module Encoder = Encoder
  module Syntax = Syntax
end

include struct
  open Dune_lang
  module Package_name = Package_name
  module Action = Action
  module String_with_vars = String_with_vars
  module Pform = Pform
  module Blang = Dune_lang.Blang
  module Slang = Dune_lang.Slang
end
