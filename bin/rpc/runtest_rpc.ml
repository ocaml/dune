open! Import

let runtest ~wait ~dir_or_cram_test_paths =
  Rpc_common.fire_request
    ~name:"runtest"
    ~wait
    Dune_rpc_impl.Decl.runtest
    dir_or_cram_test_paths
;;
