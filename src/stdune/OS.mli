(** Same as [Sys.os_type] but typed *)

module Type : sig
  type windows = Windows
  type unix = Unix
  type 'a t = private
    | Windows : windows t
    | Unix : unix t
end

type t
val t : t Type.t
