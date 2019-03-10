open Stdune
open Build.O

type t = unit

let make ?requires:_ ?flags:_ ?preprocess:_
      ?libname:_ ?source_dirs:_ ?objs_dirs () = ()

let add_source_dir x _ = x

let merge_all _ = None

let merlin_exists = Build.arr (fun x -> x)

let add_rules _ ~dir:_ ~more_src_dirs:_ ~expander:_
      ~dir_kind:_ () = ()
