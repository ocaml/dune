include Stdune
include Dune_config
include Dune_vcs
include Dune_util
module Console = Dune_console

include struct
  open Dune_engine
  module Execution_parameters = Execution_parameters
  module Compound_user_error = Compound_user_error
  module Fs_memo = Fs_memo
  module Build_system = Build_system
  module Fs_cache = Fs_cache
  include No_io
end

let phys_equal x y = x == y

include struct
  open Dune_digest
  module Digest = Digest
end

include struct
  open Dune_lang
  module Profile = Profile
  module Subst_config = Subst_config
  module Format_config = Format_config
  module Stanza = Stanza
  module String_with_vars = String_with_vars
  module Pform = Pform
  module Glob = Glob
  module Value = Value
  module Dep_conf = Dep_conf
  module Package_version = Package_version
  module Toggle = Toggle
  module Site = Site
  module Warning = Warning
  module Source_kind = Source_kind
  module Package_info = Package_info
  module Section = Section
  module Package_dependency = Package_dependency
  module Package_constraint = Package_constraint
  module Dune_project_name = Dune_project_name
  module Package = Package
  module Dialect = Dialect
end
