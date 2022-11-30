(** An augmented context *)

(** A context augmented with: a lib-db, ... Super context are used for
    generating rules. *)

open Import

type t

val all : t Context_name.Map.t Memo.Lazy.t

(** Find a super context by name. *)
val find : Context_name.t -> t option Memo.t

val to_dyn : t -> Dyn.t

val context : t -> Context.t

(** Context env with additional variables computed from packages *)
val context_env : t -> Env.t

val build_dir_is_vendored : Path.Build.t -> bool Memo.t

val with_vendored_flags :
  ocaml_version:Version.t -> Ocaml_flags.t -> Ocaml_flags.t

(** Compute the ocaml flags based on the directory environment and a buildable
    stanza *)
val ocaml_flags :
  t -> dir:Path.Build.t -> Ocaml_flags.Spec.t -> Ocaml_flags.t Memo.t

val js_of_ocaml_runtest_alias : t -> dir:Path.Build.t -> Alias.Name.t Memo.t

val js_of_ocaml_compilation_mode :
  t -> dir:Path.Build.t -> Js_of_ocaml.Compilation_mode.t Memo.t

val js_of_ocaml_flags :
     t
  -> dir:Path.Build.t
  -> Js_of_ocaml.Flags.Spec.t
  -> string list Action_builder.t Js_of_ocaml.Flags.t Memo.t

val default_foreign_flags :
     t
  -> dir:Path.Build.t
  -> language:Foreign_language.t
  -> string list Action_builder.t

val foreign_flags :
     t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> flags:Ordered_set_lang.Unexpanded.t
  -> language:Foreign_language.t
  -> string list Action_builder.t

val link_flags :
  t -> dir:Path.Build.t -> Link_flags.Spec.t -> Link_flags.t Memo.t

val menhir_flags :
     t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> flags:Ordered_set_lang.Unexpanded.t
  -> string list Action_builder.t

(** Binaries that are symlinked in the associated .bin directory of [dir]. This
    associated directory is [Path.relative dir ".bin"] *)
val local_binaries :
  t -> dir:Path.Build.t -> File_binding.Expanded.t list Memo.t

val env_node : t -> dir:Path.Build.t -> Env_node.t Memo.t

(** odoc config in the corresponding [(env)] stanza. *)
val odoc : t -> dir:Path.Build.t -> Env_node.Odoc.t Memo.t

(** coq config in the corresponding [(env)] stanza. *)
val coq : t -> dir:Path.Build.t -> Env_node.Coq.t Action_builder.t Memo.t

(** Formatting settings in the corresponding [(env)] stanza. *)
val format_config : t -> dir:Path.Build.t -> Format_config.t Memo.t

(** Dump a directory environment in a readable form *)
val dump_env : t -> dir:Path.Build.t -> Dune_lang.t list Action_builder.t

val add_rule :
     t
  -> ?mode:Rule.Mode.t
  -> ?loc:Loc.t
  -> dir:Path.Build.t
  -> Action.Full.t Action_builder.With_targets.t
  -> unit Memo.t

val add_rule_get_targets :
     t
  -> ?mode:Rule.Mode.t
  -> ?loc:Loc.t
  -> dir:Path.Build.t
  -> Action.Full.t Action_builder.With_targets.t
  -> Targets.Validated.t Memo.t

val add_rules :
     t
  -> ?loc:Loc.t
  -> dir:Path.Build.t
  -> Action.Full.t Action_builder.With_targets.t list
  -> unit Memo.t

val add_alias_action :
     t
  -> Alias.t
  -> dir:Path.Build.t
  -> loc:Loc.t option
  -> Action.Full.t Action_builder.t
  -> unit Memo.t

(** [resolve_program t ?hint name] resolves a program. [name] is looked up in
    the workspace, if it is not found in the tree is is looked up in the PATH.
    If it is not found at all, the resulting [Action.Prog.t] will either return
    the resolved path or a record with details about the error and possibly a
    hint.

    [hint] should tell the user what to install when the program is not found. *)
val resolve_program :
     t
  -> dir:Path.Build.t
  -> ?hint:string
  -> loc:Loc.t option
  -> string
  -> Action.Prog.t Memo.t

val expander : t -> dir:Path.Build.t -> Expander.t Memo.t

module As_memo_key : sig
  include Memo.Input with type t = t

  module And_package : Memo.Input with type t = t * Package.t
end
