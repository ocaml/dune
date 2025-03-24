open Import

module Key : sig
  (* This module implements a bi-directional function between [encoded] and
     [decoded] *)
  type encoded = Digest.t

  module Decoded : sig
    type t = private
      { pps : Lib_name.t list
      ; project_root : Path.Source.t option
      }

    val of_libs : Lib.t list -> t
  end

  (* [decode y] fails if there hasn't been a previous call to [encode] such that
     [encode x = y]. *)
  val encode : Decoded.t -> encoded
  val decode : encoded -> Decoded.t
end

module Driver : sig
  type t

  type loc =
    | User_file of Loc.t * (Loc.t * Lib_name.t) list
    | Dot_ppx of Path.Build.t * Lib_name.t list

  val select : Lib.t list -> loc:loc -> t Resolve.t Memo.t
  val flags : t -> Ordered_set_lang.Unexpanded.t
  val as_ppx_flags : t -> Ordered_set_lang.Unexpanded.t
  val lint_flags : t -> Ordered_set_lang.Unexpanded.t
end

val build_ppx_driver
  :  Super_context.t
  -> scope:Scope.t
  -> target:Path.Build.t
  -> pps:Lib.t list Resolve.t
  -> pp_names:Lib_name.t list
  -> unit Memo.t

val ppx_driver_and_flags
  :  Context.t
  -> lib_name:Lib_name.Local.t option
  -> expander:Expander.t
  -> scope:Scope.t
  -> loc:Loc.t
  -> flags:String_with_vars.t list
  -> (Loc.t * Lib_name.t) list
  -> (Path.Build.t * Driver.t * string list) Action_builder.t

val ppx_exe_path : Build_context.t -> key:string -> Path.Build.t
val ppx_exe : Context.t -> scope:Scope.t -> Lib_name.t -> Path.Build.t Resolve.Memo.t

(** Get a path to a cached ppx driver with some extra flags for cookies. *)
val get_ppx_driver
  :  Context.t
  -> loc:Loc.t
  -> expander:Expander.t
  -> scope:Scope.t
  -> lib_name:Lib_name.Local.t option
  -> flags:String_with_vars.t list
  -> (Loc.t * Lib_name.t) list
  -> (Path.Build.t * string list) Action_builder.t
