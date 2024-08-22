open Stdune

let initialized = ref false
let toggled = ref []

let init ~names =
  if !initialized then Code_error.raise "Compile_time.init: already initialized" [];
  toggled := names;
  initialized := true
;;

let make name =
  Config.make_toggle
    ~name
    ~default:(if List.exists ~f:(String.equal name) !toggled then `Enabled else `Disabled)
;;

let toolchains = make "toolchains"
let pkg_build_progress = make "pkg_build_progress"
