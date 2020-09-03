open! Dune_engine
open! Stdune
open Import
open Dune_file

let mlds_by_package_def =
  let module Output = struct
    type t = Path.Build.t list Package.Name.Map.t

    let to_dyn _ = Dyn.Opaque
  end in
  Memo.With_implicit_output.create "mlds by package"
    ~implicit_output:Rules.implicit_output ~doc:"mlds by package"
    ~input:(module Super_context.As_memo_key)
    ~output:(module Output)
    ~visibility:Hidden Sync
    (fun sctx ->
      let stanzas = Super_context.stanzas sctx in
      stanzas
      |> List.concat_map ~f:(fun (w : _ Dir_with_dune.t) ->
             List.filter_map w.data ~f:(function
               | Documentation d ->
                 let dc = Dir_contents.get sctx ~dir:w.ctx_dir in
                 let mlds = Dir_contents.mlds dc d in
                 Some (d.package.name, mlds)
               | _ -> None))
      |> Package.Name.Map.of_list_reduce ~f:List.rev_append)

let mlds_by_package = Memo.With_implicit_output.exec mlds_by_package_def

(* TODO memoize this so that we can cutoff at the package *)
let mlds sctx pkg =
  Package.Name.Map.find (mlds_by_package sctx) pkg |> Option.value ~default:[]
