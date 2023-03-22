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
  module Info : sig
    type t =
      { loc : Loc.t
      ; flags : Ordered_set_lang.Unexpanded.t
      ; as_ppx_flags : Ordered_set_lang.Unexpanded.t
      ; lint_flags : Ordered_set_lang.Unexpanded.t
      ; main : string
      ; replaces : (Loc.t * Lib_name.t) list
      }
  end

  type t

  (* Where are we called from? *)
  type loc =
    | User_file of Loc.t * (Loc.t * Lib_name.t) list
    | Dot_ppx of Path.Build.t * Lib_name.t list

  val info : t -> Info.t

  val select : Lib.t list -> loc:loc -> t Resolve.t Memo.t
end

val ppx_exe : Context.t -> key:string -> Path.Build.t

val ppx_driver_exe : Context.t -> Lib.t list -> Path.Build.t

val driver_flags :
     Expander.t
  -> corrected_suffix:string
  -> driver_flags:Ordered_set_lang.Unexpanded.t
  -> standard:string list Action_builder.t
  -> string list Action_builder.t

(** Get a path to a cached ppx driver with some extra flags for cookies. *)
val get_ppx_driver :
     Super_context.t
  -> loc:Loc.t
  -> expander:Expander.t
  -> scope:Scope.t
  -> lib_name:Lib_name.Local.t option
  -> flags:String_with_vars.t list
  -> (Loc.t * Lib_name.t) list
  -> (Path.Build.t * string list) Action_builder.t

val ppx_exe_driver_and_flags :
     context:Context.t
  -> expander:Expander.t
  -> scope:Scope.t
  -> loc:Loc.t
  -> lib_name:Lib_name.Local.t option
  -> flags:String_with_vars.t list
  -> (Loc.t * Lib_name.t) list
  -> (Path.Build.w Path.Local_gen.t * Driver.t * string list) Action_builder.t
