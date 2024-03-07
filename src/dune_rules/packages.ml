open Import
open Memo.O

(* CR-someday jeremiedimino: This should be a memoized function [Super_context.t
   -> Package.t -> Path.Build.t list Memo.t]. *)
let mlds_by_package_def =
  Memo.With_implicit_output.create
    "mlds by package"
    ~implicit_output:Rules.implicit_output
    ~input:(module Super_context.As_memo_key)
    (fun sctx ->
      let ctx = Super_context.context sctx in
      Context.name ctx
      |> Dune_load.dune_files
      >>= Memo.parallel_map ~f:(fun dune_file ->
        Dune_file.stanzas dune_file
        >>= Memo.parallel_map ~f:(fun stanza ->
          match Stanza.repr stanza with
          | Documentation.T stanza ->
            let+ mlds =
              (let dir =
                 Path.Build.append_source
                   (Context.build_dir ctx)
                   (Dune_file.dir dune_file)
               in
               Dir_contents.get sctx ~dir)
              >>= Dir_contents.mlds ~stanza
            in
            let name = Package.name stanza.package in
            Some (name, mlds)
          | _ -> Memo.return None)
        >>| List.filter_opt)
      >>| List.concat
      >>| Package.Name.Map.of_list_reduce ~f:List.rev_append)
;;

let mlds_by_package = Memo.With_implicit_output.exec mlds_by_package_def

(* TODO memoize this so that we can cutoff at the package *)
let mlds sctx pkg =
  let+ map = mlds_by_package sctx in
  Package.Name.Map.find map pkg |> Option.value ~default:[]
;;
