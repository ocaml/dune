include Stdune
include Dune_vcs
module Console = Dune_console
module Digest = Dune_digest

include struct
  open Dune_findlib.Findlib
  module Dune_findlib = Dune_findlib.Findlib
  module Findlib_config = Config
  module Meta = Meta
end

include struct
  open Dune_util
  module Execution_env = Execution_env
  module Log = Log
  module Persistent = Persistent
  module Stringlike = Stringlike

  module type Stringlike = Stringlike
end

include Dune_config
include Dune_config_file

include struct
  open Dune_engine
  module Dir_set = Dir_set
  module Rule = Rule
  module Rules = Rules
  module Build_system = Build_system
  module Context_name = Context_name
  module Dpath = Dpath
  module Alias = Alias
  module File_selector = File_selector
  module Dep = Dep
  module Build_config = Build_config
  module Fs_memo = Fs_memo
  module Sandbox_config = Sandbox_config
  module Sandbox_mode = Sandbox_mode
  module Action = Action
  module Compound_user_error = Compound_user_error
  module Fs_cache = Fs_cache
  module Process = Process
  module Execution_parameters = Execution_parameters
  module Build_context = Build_context
  module Targets = Targets
  module Utils = Utils
  module Load_rules = Load_rules
  module Response_file = Response_file
  module Subdir_set = Subdir_set
end

include struct
  open Ocaml
  module Cm_kind = Cm_kind
  module Mode = Mode
  module Ml_kind = Ml_kind
  module Variant = Variant
  module Version = Version
end

module Re = Dune_re
module Syntax = Dune_sexp.Syntax

include struct
  open Dune_lang
  module Lib_name = Lib_name
  module Wrapped = Wrapped
  module Targets_spec = Targets_spec
  module Profile = Profile
  module Locks = Locks
  module Subst_config = Subst_config
  module Bindings = Bindings
  module Format_config = Format_config
  module Lib_kind = Lib_kind
  module Lib_dep = Lib_dep
  module Ordered_set_lang = Ordered_set_lang
  module Stanza = Stanza
  module String_with_vars = String_with_vars
  module Pform = Pform
  module Glob = Glob
  module Diff = Action.Diff
  module Outputs = Action.Outputs
  module Value = Value
  module Blang = Blang
  module Slang = Slang
  module Binary_kind = Binary_kind
  module Visibility = Visibility
  module Dep_conf = Dep_conf
  module Package_version = Package_version
  module Relop = Relop
  module Package_variable_name = Package_variable_name
  module Toggle = Toggle
  module Site = Site
  module Warning = Warning
  module Source_kind = Source_kind
  module Package_info = Package_info
  module Section = Section
  module Package_dependency = Package_dependency
  module Package_constraint = Package_constraint
end

include Dune_engine.No_io

module Build_config = struct
  module Gen_rules = struct
    open Build_config.Gen_rules
    module Build_only_sub_dirs = Build_only_sub_dirs
    module Rules = Rules

    let make
      ?(build_dir_only_sub_dirs = Rules.empty.build_dir_only_sub_dirs)
      ?(directory_targets = Rules.empty.directory_targets)
      rules
      =
      let rules = { Rules.build_dir_only_sub_dirs; directory_targets; rules } in
      Gen_rules_result.rules_here rules
    ;;

    include Gen_rules_result

    type result = Gen_rules_result.t

    module type Generator = Rule_generator

    let rules_for ?directory_targets ~dir ~allowed_subdirs rules =
      Rules.create
        ?directory_targets
        ~build_dir_only_sub_dirs:
          (Build_only_sub_dirs.singleton ~dir (Subdir_set.of_set allowed_subdirs))
        rules
    ;;

    let map_rules t ~f =
      match t with
      | Unknown_context -> Unknown_context
      | Rules rules -> Rules (f rules)
      | Redirect_to_parent rules -> Redirect_to_parent (f rules)
    ;;

    let combine x y =
      match x, y with
      | Unknown_context, _ -> Unknown_context
      | _, Unknown_context -> Unknown_context
      | Rules x, Rules y -> Rules (Rules.combine_exn x y)
      | Rules x, Redirect_to_parent y -> Redirect_to_parent (Rules.combine_exn x y)
      | Redirect_to_parent x, Rules y -> Redirect_to_parent (Rules.combine_exn x y)
      | Redirect_to_parent x, Redirect_to_parent y ->
        Redirect_to_parent (Rules.combine_exn x y)
    ;;
  end

  let set = Build_config.set
end

let phys_equal x y = x == y
let ( == ) = `Use_phys_equal

(** Controls whether we use background threads in the dune rules *)
let background_dune_rules =
  Config.make ~name:"background_dune_rules" ~of_string:Toggle.of_string ~default:`Disabled
;;
