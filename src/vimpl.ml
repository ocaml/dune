open! Stdune

type t =
  { vlib                 : Lib.t
  ; impl                 : Dune_file.Library.t
  ; obj_dir              : Path.Build.t Obj_dir.t
  ; vlib_modules         : Lib_modules.t
  ; vlib_foreign_objects : Path.t list
  ; vlib_dep_graph       : Dep_graph.Ml_kind.t
  }

let vlib_modules t = t.vlib_modules
let vlib t = t.vlib
let impl t = t.impl
let vlib_dep_graph t = t.vlib_dep_graph

let make ~vlib ~impl ~dir ~vlib_modules ~vlib_foreign_objects ~vlib_dep_graph =
  { impl
  ; obj_dir = Dune_file.Library.obj_dir ~dir impl
  ; vlib
  ; vlib_modules
  ; vlib_dep_graph
  ; vlib_foreign_objects
  }

let vlib_stubs_o_files = function
  | None -> []
  | Some t -> t.vlib_foreign_objects
