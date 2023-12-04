(** Create generic mappings between action ast's *)

module Make (Src : Action_intf.Ast) (Dst : Action_intf.Ast) : sig
  type map =
    Src.t
    -> dir:Src.path
    -> f_program:(dir:Src.path -> Src.program -> Dst.program)
    -> f_string:(dir:Src.path -> Src.string -> Dst.string)
    -> f_path:(dir:Src.path -> Src.path -> Dst.path)
    -> f_target:(dir:Src.path -> Src.target -> Dst.target)
    -> f_ext:(dir:Src.path -> Src.ext -> Dst.ext)
    -> Dst.t

  val map_one_step : map -> map
  val map : map
end
