(** Dune files that are installed on the system *)

open! Stdune

module Virtual_library : sig
  type t =
    { virtual_modules : Module.t Module.Name.Map.t
    ; lib_name : Lib_name.Local.t
    ; wrapped : bool
    }
end

type 'subsystem t =
  { virtual_library : Virtual_library.t option
  ; sub_systems     : 'subsystem Sub_system_name.Map.t
  }

val load : Path.t -> Dune_file.Sub_system_info.t t
val gen
  :  dune_version:Syntax.Version.t
  -> (Syntax.Version.t * Dsexp.t) t
  -> Dsexp.t
