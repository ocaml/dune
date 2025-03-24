(** Mock ocaml-index real CLI *)

open Cmdliner

let touch file =
  let chan = open_out file in
  close_out chan
;;

module Common = struct
  let set_log_level _ _ = ()

  let verbose =
    let doc = "increase log verbosity" in
    Arg.(value & flag & info [ "v"; "verbose" ] ~doc)
  ;;

  let debug =
    let doc = "set maximum log verbosity" in
    Arg.(value & flag & info [ "debug" ] ~doc)
  ;;

  let with_log = Term.(const set_log_level $ debug $ verbose)

  let output_file =
    let doc = "name of the generated index" in
    Arg.(value & opt string "project.ocaml-index" & info [ "o"; "output-file" ] ~doc)
  ;;
end

module Aggregate = struct
  let from_files _ _ output_file _ _ _ () = touch output_file

  let root =
    let doc = "if provided all locations will be appended to that path" in
    Arg.(value & opt (some string) None & info [ "root" ] ~doc)
  ;;

  let files =
    let doc = "the files to index" in
    Arg.(value & pos_all string [] & info [] ~doc)
  ;;

  let build_path =
    let doc = "an extra directory to add to the load path" in
    Arg.(value & opt_all string [] & info [ "I" ] ~doc)
  ;;

  let store_shapes =
    let doc = "aggregate input-indexes shapes and store them in the new index" in
    Arg.(value & flag & info [ "store-shapes" ] ~doc)
  ;;

  let no_cmt =
    let doc = "" in
    Arg.(value & flag & info [ "no-cmt-load-path" ] ~doc)
  ;;

  let term =
    Term.(
      const from_files
      $ store_shapes
      $ root
      $ Common.output_file
      $ build_path
      $ no_cmt
      $ files
      $ Common.with_log)
  ;;

  let cmd =
    let info =
      let doc = "builds the index for a single $(i, .cmt) file" in
      Cmd.info "aggregate" ~doc
    in
    Cmd.v info term
  ;;
end

module Dump = struct
  let dump file () = Printf.printf "Dump %s" file

  let file =
    let doc = "the file to dump" in
    Arg.(required & pos 0 (some string) None & info [] ~doc)
  ;;

  let term = Term.(const dump $ file $ Common.with_log)

  let cmd =
    let info =
      let doc = "print the content of an index file to stdout" in
      Cmd.info "dump" ~doc
    in
    Cmd.v info term
  ;;
end

let subcommands =
  let info =
    let doc = "An indexer for OCaml's artifacts" in
    Cmd.info "ocaml-index" ~doc
  in
  Cmd.group info ~default:Aggregate.term [ Aggregate.cmd; Dump.cmd ]
;;

let () = exit (Cmd.eval subcommands)
