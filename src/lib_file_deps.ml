open Stdune

let cmi lib =
  { Arg_spec.
    dir = Lib.obj_dir lib
  ; exts = [".cmi"]
  }

let cmi_and_cmx lib =
  let cmi = cmi lib in
  { cmi with exts = ".cmx" :: cmi.exts }

module L = struct
  let file_deps_of_lib (lib : Lib.t) ~exts =
    { Arg_spec.
      dir = Lib.obj_dir lib
    ; exts
    }

  let headers =
    List.map ~f:(fun lib ->
      { Arg_spec.
        dir = Lib.src_dir lib
      ; exts = [".h"]
      })

  let file_deps_with_exts lib_exts =
    List.rev_map lib_exts ~f:(fun (lib, exts) -> file_deps_of_lib lib ~exts)

  let file_deps libs ~exts =
    List.rev_map libs ~f:(file_deps_of_lib ~exts)

  let cmi         = List.map ~f:cmi
  let cmi_and_cmx = List.map ~f:cmi_and_cmx
end
