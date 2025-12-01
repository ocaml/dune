open Import

include module type of struct
  include Rule_mode
end

val expand_path : t -> expander:Expander.t -> dir:Path.Build.t -> Rule.Mode.t Memo.t
val expand_str : t -> expander:Expander.t -> Rule.Mode.t Memo.t
