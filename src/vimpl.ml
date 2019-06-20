open! Stdune

type t =
  { vlib                 : Lib.t
  ; impl                 : Dune_file.Library.t
  ; vlib_modules         : Modules.t
  ; vlib_foreign_objects : Path.t list
  ; vlib_dep_graph       : Dep_graph.Ml_kind.t
  }

let vlib_modules t = t.vlib_modules
let vlib t = t.vlib
let impl t = t.impl
let vlib_dep_graph t = t.vlib_dep_graph
let impl_modules t m =
  match t with
  | None -> m
  | Some t -> Modules.impl ~vlib:t.vlib_modules m

let make ~vlib ~impl ~vlib_modules ~vlib_foreign_objects ~vlib_dep_graph =
  { impl
  ; vlib
  ; vlib_modules
  ; vlib_dep_graph
  ; vlib_foreign_objects
  }

let vlib_stubs_o_files = function
  | None -> []
  | Some t -> t.vlib_foreign_objects
