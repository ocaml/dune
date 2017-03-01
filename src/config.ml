open! Import

let local_install_dir =
  let dir = Path.(relative root) "install" in
  fun ~context -> Path.relative dir context
