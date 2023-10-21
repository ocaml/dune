open Import

module Solver_result : sig
  type t =
    { lock_dir : Lock_dir.t
    ; files : Lock_dir.Write_disk.Files_entry.t Package_name.Map.Multi.t
    }
end

val solve_lock_dir
  :  Solver_env.t
  -> Version_preference.t
  -> Opam_repo.t list
  -> local_packages:Opam_repo.With_file.t Package_name.Map.t
  -> experimental_translate_opam_filters:bool
  -> (Solver_result.t, [ `Diagnostic_message of _ Pp.t ]) result Fiber.t
