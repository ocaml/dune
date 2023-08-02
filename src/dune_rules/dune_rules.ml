module Meta = Meta
module Findlib = Findlib
module Main = Main
module Context = Context
module Super_context = Super_context
module Compilation_context = Compilation_context
module Colors = Colors
module Workspace = Workspace
module Dune_package = Dune_package
module Alias_rec = Alias_rec
module Dep_conf = Dep_conf
module Dir_contents = Dir_contents
module Expander = Expander
module Lib = Lib
module Lib_dep = Lib_dep
module Lib_flags = Lib_flags
module Lib_info = Lib_info
module Modules = Modules
module Module_compilation = Module_compilation
module Exe_rules = Exe_rules
module Lib_rules = Lib_rules
module Obj_dir = Obj_dir
module Merlin_ident = Merlin_ident
module Merlin = Merlin
module Ml_sources = Ml_sources
module Scope = Scope
module Module = Module
module Module_name = Module_name
module Dune_file = Dune_file
module Artifact_substitution = Artifact_substitution
module Dune_load = Dune_load
module Opam_create = Opam_create
module Link_mode = Link_mode
module Utop = Utop
module Setup = Setup
module Toplevel = Toplevel
module Top_module = Top_module
module Global = Global
module Only_packages = Only_packages
module Resolve = Resolve
module Ocamldep = Ocamldep
module Dep_rules = Dep_rules
module Dep_graph = Dep_graph
module Preprocess = Preprocess
module Preprocessing = Preprocessing
module Coq_mode = Coq_mode
module Coq_rules = Coq_rules
module Coq_module = Coq_module
module Coq_sources = Coq_sources
module Coq_lib_name = Coq_lib_name
module Coq_lib = Coq_lib
module Command = Command
module Clflags = Clflags
module Dune_project = Dune_project
module Source_tree = Source_tree
module Sub_dirs = Sub_dirs
module Package = Package
module Dialect = Dialect

module Install_rules = struct
  let install_file = Install_rules.install_file

  let stanzas_to_entries = Install_rules.stanzas_to_entries
end

module For_tests = struct
  module Scheme = Scheme
  module Lib_config = Lib_config
  module Dynlink_supported = Dynlink_supported
  module Ocamlobjinfo = Ocamlobjinfo
  module Action_unexpanded = Action_unexpanded
  module Jsoo_rules = Jsoo_rules
end
