(* CR-someday amokhov: Add [digest_cache : bool] as well. *)

(** For each cache layer, the flag controls if dune should, on each cache miss,
    print an explanation of why lookup failed. *)
type t =
  { shared_cache : bool
  ; workspace_local_cache : bool
  ; fs_cache : bool
  }

let equal t { shared_cache; workspace_local_cache; fs_cache } =
  Bool.equal t.shared_cache shared_cache
  && Bool.equal t.workspace_local_cache workspace_local_cache
  && Bool.equal t.fs_cache fs_cache
;;
