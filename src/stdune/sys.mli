include module type of struct
  include Stdlib.Sys
end

val linux : bool
(** Are we running on linux? *)

val force_remove : string -> unit
