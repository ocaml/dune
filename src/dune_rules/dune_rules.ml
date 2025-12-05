module Alias_builder = Alias_builder
module Action_builder = Action_builder
module Alias = Alias0
module Findlib = Findlib
module Main = Main
module Context = Context
module Env_node = Env_node
module Link_flags = Link_flags
module Ocaml_flags = Ocaml_flags
module Ocaml_flags_db = Ocaml_flags_db
module Menhir_rules = Menhir_rules
module Foreign_rules = Foreign_rules
module Jsoo_rules = Jsoo_rules
module Super_context = Super_context
module Compilation_context = Compilation_context
module Colors = Colors
module Dune_package = Dune_package
module Alias_rec = Alias_rec
module Dir_contents = Dir_contents
module Expander = Expander
module Lib = Lib
module Lib_flags = Lib_flags
module Lib_info = Lib_info
module Lib_id = Lib_id
module Modules = Modules
module Exe_rules = Exe_rules
module Lib_rules = Lib_rules
module Obj_dir = Obj_dir
module Merlin_ident = Merlin_ident
module Merlin = Merlin
module Ml_sources = Ml_sources
module Scope = Scope
module Module = Module
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
module Only_packages = Source.Only_packages
module Resolve = Resolve
module Ocamldep = Ocamldep
module Dep_rules = Dep_rules
module Dep_graph = Dep_graph
module Lib_config = Lib_config
module Pp_spec = Pp_spec
module Pp_spec_rules = Pp_spec_rules
module Command = Command
module Clflags = Clflags
module Private_context = Private_context
module Odoc = Odoc
module Library = Library
module Melange = Melange
module Executables = Executables
module Tests = Tests
module Stanzas = Stanzas
module Lock_dir = Lock_dir
module Pkg_dev_tool = Pkg_dev_tool
module Pkg_build_progress = Pkg_build_progress
module Compile_time = Compile_time
module Cram_rules = Cram_rules
module Cram_stanza = Cram_stanza
module Instrumentation = Instrumentation
module Sub_system_name = Sub_system_name
module Inline_tests_info = Inline_tests_info

module Install_rules = struct
  let install_file = Install_rules.install_file
  let stanzas_to_entries = Install_rules.stanzas_to_entries
end

module Pkg_rules = struct
  let all_filtered_depexts = Pkg_rules.all_filtered_depexts
  let pkg_digest_of_project_dependency = Pkg_rules.pkg_digest_of_project_dependency

  module Pkg_digest = Pkg_rules.Pkg_digest
end

module For_tests = struct
  module Dynlink_supported = Dynlink_supported
  module Ocamlobjinfo = Ocamlobjinfo
  module Action_unexpanded = Action_unexpanded
  module Cram_exec = Cram_exec
end

module Coq = struct
  module Coq_mode = Coq_mode
  module Coq_rules = Coq_rules
  module Coq_module = Coq_module
  module Coq_sources = Coq_sources
  module Coq_lib_name = Coq_lib_name
  module Coq_lib = Coq_lib
  module Coq_flags = Coq_flags
end

module Rocq = struct
  module Rocq_mode = Rocq_mode
  module Rocq_rules = Rocq_rules
  module Rocq_module = Rocq_module
  module Rocq_sources = Rocq_sources
  module Rocq_lib_name = Rocq_lib_name
  module Rocq_lib = Rocq_lib
  module Rocq_flags = Rocq_flags
end
