(** All configuration settings of Dune's local and cloud (in future) caches. *)
type t =
  | Disabled
  | Enabled of
      { storage_mode : Dune_cache_storage.Mode.t
      ; check_probability : float
      }
