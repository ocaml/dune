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
  module Action = Action
  module String_with_vars = String_with_vars
  module Pform = Pform
  module Blang = Blang
  module Slang = Slang
  module Package_variable_name = Package_variable_name
end

include struct
  open Dune_engine
  module Process = Process
  module Display = Display
end

module OpamUrl = OpamUrl0
