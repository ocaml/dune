(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Manpage = Cmdliner_manpage
module Term = Cmdliner_term
module Cmd = struct
  module Exit = Cmdliner_def.Exit
  module Env = Cmdliner_def.Env
  include Cmdliner_cmd
  include Cmdliner_eval
end
module Arg = Cmdliner_arg
