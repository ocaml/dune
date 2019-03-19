open! Stdune
open Import
open Dune_file

let mlds_by_package_def =
  let module Output = struct
    type t = Path.t list Package.Name.Map.t

    let equal = Package.Name.Map.equal ~equal:(List.equal Path.equal)

    let to_sexp _ = Sexp.Encoder.string "opaque"
  end
  in
  Memo.create "mlds by package"
    ~doc:"mlds by package"
    ~input:(module Super_context)
    ~output:(Allow_cutoff (module Output))
    ~visibility:Hidden
    Sync
    None

let () =
  let mlds_by_package sctx =
    let stanzas = Super_context.stanzas sctx in
    stanzas
    |> List.concat_map ~f:(fun (w : _ Dir_with_dune.t) ->
      List.filter_map w.data ~f:(function
        | Documentation d ->
          let dc = Dir_contents.get sctx ~dir:w.ctx_dir in
          let mlds = Dir_contents.mlds dc d in
          Some (d.package.name, mlds)
        | _ ->
          None
      ))
    |> Package.Name.Map.of_list_reduce ~f:List.rev_append
  in
  Memo.set_impl mlds_by_package_def mlds_by_package

let mlds_by_package = Memo.exec mlds_by_package_def

(* TODO memoize this so that we can cutoff at the package *)
let mlds sctx pkg =
  Package.Name.Map.find (mlds_by_package sctx) pkg
  |> Option.value ~default:[]
