open Stdune

(* CR-someday amokhov: We should probably switch from float [check_probability]
   to integer [check_frequency], as in Jenga, to avoid generating random floats. *)

(* CR-soon amokhov: That that the reproducibility check actually works. *)

module Reproducibility_check = struct
  (* CR-someday amokhov: Add [Check_and_repair] to rewrite cache entries if they
     disagree with the check. *)
  type t =
    | Skip
    | Check of { check_probability : float }

  let sample = function
    | Skip -> false
    | Check { check_probability } -> Random.float 1. < check_probability

  let to_dyn = function
    | Skip -> Dyn.Variant ("Skip", [])
    | Check { check_probability } ->
      Dyn.Variant ("Check", [ Dyn.Float check_probability ])

  let check check_probability = Check { check_probability }
end

type t =
  | Disabled
  | Enabled of
      { storage_mode : Dune_cache_storage.Mode.t
      ; reproducibility_check : Reproducibility_check.t
      }
