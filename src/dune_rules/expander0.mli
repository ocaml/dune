open Import

val isn't_allowed_in_this_position : source:Dune_lang.Template.Pform.t -> 'a
val as_in_build_dir : what:string -> loc:Loc.t -> Path.t -> Path.Build.t

module Deps : sig
  type 'a t =
    | Without of 'a Memo.t
    | With of 'a Action_builder.t

  include Applicative with type 'a t := 'a t

  val action_builder : 'a t -> 'a Action_builder.t
  val dep : Path.t -> Value.t list Action_builder.t
end

module Expanding_what : sig
  type t =
    | Nothing_special
    | Deps_like_field
    | User_action of Path.Build.t Targets_spec.t
    | User_action_without_targets of { what : string }
end

type t

val make : dir:Path.Build.t -> Expanding_what.t -> t
val dir : t -> Path.Build.t
val context : t -> Context_name.t
val expanding_what : t -> Expanding_what.t

module Expansion_result : sig
  type value := Value.t list Deps.t

  type nonrec t =
    | Direct of value
    | Need_full_expander of (t -> value)
end

module Source : sig
  val make
    :  (Loc.t -> Expansion_result.t) Pform.Var.Map.t
    -> (Loc.t -> Pform.Macro_invocation.t -> Expansion_result.t) Pform.Macro.Map.t
    -> unit

  val expand : Loc.t -> Pform.t -> Expansion_result.t option
end
