(** The modules field evaluator interprets the module sources into proper
    modules in a directory in the context of a stanza from a dune file. *)

open Import

module Virtual : sig
  type t = { virtual_modules : Ordered_set_lang.Unexpanded.t }
end

module Implementation : sig
  type t =
    { existing_virtual_modules : Module_name.Path.Set.t
    ; allow_new_public_modules : bool
    }
end

type kind =
  | Virtual of Virtual.t
  | Implementation of Implementation.t
  | Exe_or_normal_lib
  | Parameter

val expand_only_available
  :  expander:Expander.t
  -> modules:Module.Source.t Module_trie.t
  -> module_path:Module_name.t list option
  -> stanza_loc:Loc.t
  -> Modules_settings.t
  -> (Loc.t * Module.Source.t) Module_trie.t Memo.t

val eval
  :  expander:Expander.t
  -> modules:Module.Source.t Module_trie.t
  -> module_path:Module_name.t list option
  -> stanza_loc:Loc.t
  -> private_modules:Ordered_set_lang.Unexpanded.t
  -> kind:kind
  -> src_dir:Path.Build.t
  -> version:Dune_lang.Syntax.Version.t
  -> Modules_settings.t
  -> ((Loc.t * Module.Source.t) Module_trie.t * Module.t Module_trie.t) Memo.t
