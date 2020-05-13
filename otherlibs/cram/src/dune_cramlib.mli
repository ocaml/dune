module V1 : sig
  module Sanitizer : sig
    type t

    val default : t

    val id : t

    val make : (string -> string) -> t

    val rewrite_build_path_prefix_map : t

    module O : sig
      val ( >>> ) : t -> t -> t
    end
  end

  type t

  val make : ?default_sanitizer:Sanitizer.t -> unit -> t

  val run : t -> unit
end
