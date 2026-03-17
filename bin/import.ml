include Stdune
include Dune_config_file
include Dune_vcs
include Dune_scheduler
module Targets = Dune_targets

include struct
  open Dune_engine
  module Build_config = Build_config
  module Build_system = Build_system
  module Build_system_error = Build_system_error
  module Load_rules = Load_rules
  module Hooks = Hooks
  module Action_builder = Dune_rules.Action_builder
  module Action = Action
  module Dep = Dep
  module Action_to_sh = Action_to_sh
  module Dpath = Dpath
  module Findlib = Dune_rules.Findlib
  module Diff_promotion = Diff_promotion
  module Context_name = Context_name
end

module Cached_digest = Dune_digest.Cached_digest

include struct
  open Source
  module Source_tree = Source_tree
  module Source_dir_status = Source_dir_status
  module Workspace = Workspace
end

include struct
  open Dune_rules
  module Super_context = Super_context
  module Context = Context
  module Dune_package = Dune_package
  module Resolve = Resolve
  module Dune_file = Dune_file
  module Library = Library
  module Melange = Melange
  module Executables = Executables
  module Dir_contents = Dir_contents
end

include struct
  open Cmdliner
  module Term = Term
  module Manpage = Manpage

  module Cmd = struct
    include Cmd

    let default_exits = List.map ~f:Exit_code.info Exit_code.all

    let info ?docs ?doc ?man_xrefs ?man ?envs ?version name =
      info ?docs ?doc ?man_xrefs ?man ?envs ?version ~exits:default_exits name
    ;;
  end
end

module Digest = Dune_digest
module Console = Console

include struct
  open Dune_lang
  module Stanza = Stanza
  module Profile = Profile
  module Lib_name = Lib_name
  module Package_name = Package_name
  module Package = Package
  module Package_version = Package_version
  module Source_kind = Source_kind
  module Package_info = Package_info
  module Section = Section
  module Dune_project_name = Dune_project_name
  module Dune_project = Dune_project
end

include struct
  open Dune_pkg
  module Opam_repo = Opam_repo
  module Lock_dir = Lock_dir
  module Rev_store = Rev_store
  module Resolved_package = Resolved_package
end

module Dune_rpc = Dune_rpc_private
module Graph = Dune_graph.Graph
include Let_syntax
