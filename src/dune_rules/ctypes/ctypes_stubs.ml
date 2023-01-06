open Import

let lib_deps_of_strings ~loc lst =
  List.map lst ~f:(fun lib -> Lib_dep.Direct (loc, Lib_name.of_string lib))

let libraries_needed_for_ctypes ~loc =
  let libraries = [ "ctypes"; "ctypes.stubs" ] in
  lib_deps_of_strings ~loc libraries
