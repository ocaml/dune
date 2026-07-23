open Private
module Response = Response
module Version_error = Version_error
module Call = Call
module Loc = Loc
module Target = Target
module Diagnostic = Diagnostic
module Path = Path
module Progress = Progress
module Job = Job
module Message = Message
module Where = Where
module Registry = Registry
module Ansi_color = Ansi_color
module User_message = User_message
include Public
module Client_impl = Client

module Client = struct
  module type S = Client_impl.Public

  module Make = Client_impl.Make
end
