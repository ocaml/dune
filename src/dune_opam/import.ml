include Stdune

include struct
  open Dune_lang
  module Action = Action
  module String_with_vars = String_with_vars
  module Pform = Pform
  module Blang = Blang
  module Slang = Slang
  module Package_version = Package_version
  module Package_variable_name = Package_variable_name
  module Relop = Relop
  module Decoder = Decoder
  module Encoder = Encoder
end
