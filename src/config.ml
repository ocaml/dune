open! Import

let local_install_dir =
  let dir = Path.(relative root) "_build/install" in
  fun ~context -> Path.relative dir context

let local_install_bin_dir ~context =
  Path.relative (local_install_dir ~context) "bin"

let local_install_man_dir ~context =
  Path.relative (local_install_dir ~context) "bin"

let local_install_lib_dir ~context ~package =
  Path.relative
    (Path.relative (local_install_dir ~context) "lib")
    package

let dev_null = Path.of_string (if Sys.win32 then "nul" else "/dev/null")
