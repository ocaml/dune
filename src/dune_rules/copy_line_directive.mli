open Import

module DB : sig
  val follow_while : Path.Build.t -> f:(Path.Build.t -> 'a option) -> 'a option
end

val action : Context.t -> src:Path.t -> dst:Path.Build.t -> Action.t

val builder :
     Context.t
  -> src:Path.t
  -> dst:Path.Build.t
  -> Action.Full.t Action_builder.With_targets.t
