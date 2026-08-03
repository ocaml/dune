open Stdune
module Dep_conf = Dune_lang.Dep_conf

include struct
  open Source
  module Source_tree = Source_tree
end

include struct
  open Dune_lang
  module Dune_project = Dune_project
end

include struct
  open Dune_rpc.Private
  module Procedures = Procedures
end

module Handler = Rpc.Server.Handler

let register rpc =
  let open Fiber.O in
  let () =
    let f _ (path, `Contents contents) =
      let+ version =
        Memo.run
          (let open Memo.O in
           let source_path = For_handlers.source_path_of_string path in
           let+ dir = Source_tree.nearest_dir source_path in
           let project = Source_tree.Dir.project dir in
           Dune_project.dune_version project)
      in
      Dune_lang.Format.format_string ~version contents
    in
    Handler.implement_request rpc Procedures.Public.format_dune_file f
  in
  ()
;;

(* Hardcoding the version is sufficient because this command is unstable and is
   currently only used by tests. *)
let dep_parser = Dep_conf.command_line_parser ~stanza_version:(3, 0)

let parse_build_arg s =
  Dune_lang.Decoder.parse
    dep_parser
    Univ_map.empty
    (Dune_lang.Parser.parse_string ~fname:"dune rpc" ~mode:Dune_lang.Parser.Mode.Single s)
;;
