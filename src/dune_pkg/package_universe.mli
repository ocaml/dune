open! Import

(** All of the packages in a dune project including local packages and packages
    in a lockdir. The lockdir is guaranteed to contain a valid dependency
    solution for the local packages. *)
type t = private
  { local_packages : Local_package.t Package_name.Map.t
  ; lock_dir : Lock_dir.t
  }

val create
  :  Local_package.t Package_name.Map.t
  -> Lock_dir.t
  -> (t, User_message.t) result
