open Import
open Memo.O

let mlds_def =
  Memo.With_implicit_output.create
    "mlds by package"
    ~implicit_output:Rules.implicit_output
    ~input:(module Super_context.As_memo_key.And_package_name)
    (fun (sctx, package) ->
       let ctx = Super_context.context sctx in
       Partitions.dune_files_of_package (Context.name ctx) package
       >>= Memo.parallel_map ~f:(fun dune_file ->
         Dune_file.stanzas dune_file
         >>= Memo.parallel_map ~f:(fun stanza ->
           match Stanza.repr stanza with
           | Documentation.T stanza
             when Package.Name.equal (Package.name stanza.package) package ->
             let dir = Dune_file.output_dir dune_file in
             Dir_contents.get sctx ~dir >>= Dir_contents.mlds ~stanza
           | _ -> Memo.return [])
         >>| List.concat)
       >>| List.concat)
;;

let mlds sctx pkg = Memo.With_implicit_output.exec mlds_def (sctx, pkg)
