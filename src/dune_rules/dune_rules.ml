module Main = Main
module Context = Context
module Super_context = Super_context
module Findlib = Findlib
module Colors = Colors
module Profile = Profile
module Workspace = Workspace
module Dune_package = Dune_package
module Dep_conf = Dep_conf
module Dir_contents = Dir_contents
module Expander = Expander
module Lib = Lib
module Lib_flags = Lib_flags
module Lib_info = Lib_info
module Modules = Modules
module Exe_rules = Exe_rules
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
module Meta = Meta
module Toplevel = Toplevel
module Global = Global
module Only_packages = Only_packages
module Resolve = Resolve
module Preprocess = Preprocess
module Coq_rules = Coq_rules
module Coq_module = Coq_module
module Coq_sources = Coq_sources
module Coq_stanza = Coq_stanza
module Coq_lib = Coq_lib
module Command = Command
module Install = Install
module Lib_name = Lib_name
module Diff = Dune_lang.Action.Diff

module Install_rules = struct
  let install_file = Install_rules.install_file
end

module Dep_rules = struct
  module Current_dep_gen = Dep_rules.Current_dep_gen
end

(* Only for tests *)
module Scheme = Scheme
module Lib_config = Lib_config
module Dynlink_supported = Dynlink_supported
module Lib_dep = Lib_dep
module Ocamlobjinfo = Ocamlobjinfo
module Action_unexpanded = Action_unexpanded
