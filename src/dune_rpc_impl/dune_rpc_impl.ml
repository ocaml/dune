module Decl = Decl
module Client = Client
module Server = Server
module For_handlers = For_handlers
module Private = Rpc.Private
module Watch_mode_config = Watch_mode_config
module Where = Rpc.Where
module Dune_rules_rpc = Dune_rules_rpc

module Diagnostics = struct
  module For_tests = struct
    let diagnostic_of_error = Diagnostics.diagnostic_of_error
  end
end
