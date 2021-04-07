(* CR-someday amokhov: We should probably switch from float [check_probability]
   to integer [check_frequency], as in Jenga, to avoid generating random floats. *)

type t =
  | Disabled
  | Enabled of
      { storage_mode : Dune_cache_storage.Mode.t
      ; check_probability : float
      }
