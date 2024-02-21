open Stdune

let bin =
  lazy
    (match Bin.which ~path:(Env_path.path Env.initial) "tar" with
     | Some x -> x
     | None -> Dune_engine.Utils.program_not_found "tar" ~loc:None)
;;
