(** Simple glob support library. *)

module V1 : sig
  include module type of struct
    include Glob
  end
end
