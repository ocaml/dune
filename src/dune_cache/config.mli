type t =
  | Disabled
  | Enabled of
      { storage_mode : Dune_cache_storage.Mode.t
      ; check_probability : float
      }
