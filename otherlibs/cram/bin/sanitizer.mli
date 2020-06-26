open Stdune

module Command : sig
  type t =
    { output : string
    ; build_path_prefix_map : string
    }
end

val impl_sanitizer : (Command.t -> string) -> in_channel -> out_channel -> unit

val run_sanitizer :
     ?temp_dir:Path.t
  -> prog:string
  -> argv:string list
  -> Command.t list
  -> string list
