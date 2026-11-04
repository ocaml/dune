open Import

module DB : sig
  val follow : Path.Build.t -> Path.Build.t list
end

val action : Context.t -> src:Path.t -> dst:Path.Build.t -> Action.t

val builder
  :  Context.t
  -> src:Path.t
  -> dst:Path.Build.t
  -> Action.Full.t Action_builder.With_targets.t
