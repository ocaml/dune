include struct
  open Dune_rules
  module Dep_conf = Dep_conf
  module Source_tree = Source_tree
  module Dune_project = Dune_project
end

include struct
  open Dune_rpc_private
  module Procedures = Procedures
end

include struct
  open Dune_rpc_server
  module Handler = Handler
end

let register rpc =
  let open Fiber.O in
  let () =
    let f _ (path, `Contents contents) =
      let+ version =
        Memo.run
          (let open Memo.O in
          let source_path =
            Dune_rpc_impl.For_handlers.source_path_of_string path
          in
          let+ dir = Source_tree.nearest_dir source_path in
          let project = Source_tree.Dir.project dir in
          Dune_project.dune_version project)
      in
      Dune_lang.Format.format_string ~version contents
    in
    Handler.implement_request rpc Procedures.Public.format_dune_file f
  in
  ()
