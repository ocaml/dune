open Stdune
module Dep_conf = Dune_lang.Dep_conf

include struct
  open Dune_rules
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
           let source_path = Dune_rpc_impl.For_handlers.source_path_of_string path in
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

include struct
  open Dune_lang
  module Stanza = Stanza
  module String_with_vars = String_with_vars
  module Pform = Pform
end

(* TODO un-copy-paste from dune/bin/arg.ml *)
let dep_parser =
  Dune_lang.Syntax.set Stanza.syntax (Active Stanza.latest_version) Dep_conf.decode
;;

let parse_build s =
  Dune_lang.Decoder.parse
    dep_parser
    (Univ_map.set
       Univ_map.empty
       String_with_vars.decoding_env_key
       (* CR-someday aalekseyev: hardcoding the version here is not
          ideal, but it will do for now since this command is not
          stable and we're only using it in tests. *)
       (Pform.Env.initial (3, 0)))
    (Dune_lang.Parser.parse_string ~fname:"dune rpc" ~mode:Dune_lang.Parser.Mode.Single s)
;;
