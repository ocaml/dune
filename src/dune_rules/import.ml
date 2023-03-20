include Stdune
open Dune_util
module Digest = Dune_digest
module Console = Dune_console
module Config = Config
module Log = Log
module Persistent = Persistent
module Stringlike = Stringlike
module Stringlike_intf = Stringlike_intf
module Value = Value
include Dune_config_file

include struct
  open Dune_engine
  module Dir_set = Dir_set
  module Rule = Rule
  module Rules = Rules
  module Action_builder = Action_builder
  module Source_tree = Source_tree
  module Dialect = Dialect
  module Dune_project = Dune_project
  module Build_system = Build_system
  module Context_name = Context_name
  module Package = Package
  module Dpath = Dpath
  module Alias = Alias
  module Section = Section
  module Opam_file = Opam_file
  module File_selector = File_selector
  module Dep = Dep
  module Build_config = Build_config
  module Fs_memo = Fs_memo
  module Sandbox_config = Sandbox_config
  module Sandbox_mode = Sandbox_mode
  module Action = Action
  module Compound_user_error = Compound_user_error
  module Fs_cache = Fs_cache
  module Format_config = Format_config
  module Process = Process
  module Execution_parameters = Execution_parameters
  module Build_context = Build_context
  module Targets = Targets
  module Utils = Utils
  module Sub_dirs = Sub_dirs
  module Subst_config = Subst_config
  module Load_rules = Load_rules
  module Subdir_set = Subdir_set
  module Include_stanza = Include_stanza
  module Cram_test = Cram_test
  module Vcs = Vcs
  module Response_file = Response_file
end

include Ocaml
module Re = Dune_re
module Stanza = Dune_lang.Stanza
module Predicate_lang = Dune_lang.Predicate_lang
module Predicate_with_id = Dune_engine.File_selector.Predicate_with_id
module String_with_vars = Dune_lang.String_with_vars
module Pform = Dune_lang.Pform
module Glob = Dune_lang.Glob
module Diff = Dune_lang.Action.Diff
module Outputs = Dune_lang.Action.Outputs
module Syntax = Dune_sexp.Syntax
include Dune_engine.No_io
