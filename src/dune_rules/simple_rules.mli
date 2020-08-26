(** Simple rules: user, copy_files, alias *)
open! Dune_engine

open! Stdune
open Import
open Dune_file

module Alias_rules : sig
  val add :
       Super_context.t
    -> alias:Alias.t
    -> stamp:'a
    -> loc:Loc.t option
    -> locks:Path.t list
    -> Action.t Build.With_targets.t
    -> unit

  val add_empty :
       Super_context.t
    -> loc:Stdune.Loc.t option
    -> alias:Alias.t
    -> stamp:'a
    -> unit
end

(** Interpret a [(rule ...)] stanza and return the targets it produces. *)
val user_rule :
     Super_context.t
  -> ?extra_bindings:Pform.Map.t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> Rule.t
  -> Path.Build.Set.t

(** Interpret a [(copy_files ...)] stanza and return the targets it produces. *)
val copy_files :
     Super_context.t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> src_dir:Path.Source.t
  -> Copy_files.t
  -> Path.Set.t

(** Interpret an [(alias ...)] stanza. *)
val alias :
     Super_context.t
  -> ?extra_bindings:Pform.Map.t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> Alias_conf.t
  -> unit
