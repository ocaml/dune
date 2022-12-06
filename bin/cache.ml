open Stdune
open Import

(* CR-someday amokhov: Implement other commands supported by Jenga. *)

let trim =
  let info =
    let doc = "Trim the Dune cache" in
    let man =
      [ `P "Trim the Dune cache to a specified size or by a specified amount."
      ; `S "EXAMPLES"
      ; `Pre
          {|Trimming the Dune cache to 1 GB.

           \$ dune cache trim --trimmed-size=1GB |}
      ; `Pre
          {|Trimming 500 MB from the Dune cache.

           \$ dune cache trim --size=500MB |}
      ]
    in
    Cmd.info "trim" ~doc ~man
  in
  Cmd.v info
  @@ let+ trimmed_size =
       Arg.(
         value
         & opt (some bytes) None
         & info ~docv:"BYTES" [ "trimmed-size" ]
             ~doc:"Size to trim from the cache.")
     and+ size =
       Arg.(
         value
         & opt (some bytes) None
         & info ~docv:"BYTES" [ "size" ] ~doc:"Size to trim the cache to.")
     in
     Log.init_disabled ();
     let open Result.O in
     match
       let+ goal =
         match (trimmed_size, size) with
         | Some trimmed_size, None -> Result.Ok trimmed_size
         | None, Some size ->
           Result.Ok (Int64.sub (Dune_cache.Trimmer.overhead_size ()) size)
         | _ -> Result.Error "please specify either --size or --trimmed-size"
       in
       Dune_cache.Trimmer.trim ~goal
     with
     | Error s -> User_error.raise [ Pp.text s ]
     | Ok { trimmed_bytes } ->
       User_message.print
         (User_message.make
            [ Pp.textf "Freed %s" (Bytes_unit.pp trimmed_bytes) ])

let command =
  let info =
    let doc = "Manage the shared cache of build artifacts" in
    let man =
      [ `S "DESCRIPTION"
      ; `P
          "Dune can share build artifacts between workspaces. Currently, the \
           only action supported by this command is `trim`, but we plan to \
           provide more functionality soon."
      ]
    in
    Cmd.info "cache" ~doc ~man
  in
  Cmd.group info [ trim ]
