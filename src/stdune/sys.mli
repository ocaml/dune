include module type of struct
  include Stdlib.Sys
end

(** Are we running on linux? *)
val linux : bool

val force_remove : string -> unit
