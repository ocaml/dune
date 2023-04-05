open Stdune
open Import

(* CR-someday amokhov: Implement other commands supported by Jenga. *)

let trim =
  let info =
    let doc = "Trim the Dune cache." in
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

let size =
  let info =
    let doc = "Query the size of the Dune cache." in
    let man =
      [ `P
          "Compute the total size of files in the Dune cache which are not \
           hardlinked from any build directory and output it in a \
           human-readable form."
      ]
    in
    Cmd.info "size" ~doc ~man
  in
  Cmd.v info
  @@ let+ machine_readble =
       Arg.(
         value & flag
         & info [ "machine-readable" ]
             ~doc:"Outputs size as a plain number of bytes.")
     in
     let size = Dune_cache.Trimmer.overhead_size () in
     if machine_readble then
       User_message.print (User_message.make [ Pp.textf "%Ld" size ])
     else
       User_message.print
         (User_message.make [ Pp.textf "%s" (Bytes_unit.pp size) ])

let command =
  let info =
    let doc = "Manage Dune's shared cache of build artifacts." in
    let man =
      [ `S "DESCRIPTION"
      ; `P
          "Dune can share build artifacts between workspaces. We currently \
           only support a few subcommands; however, we plan to provide more \
           functionality soon."
      ]
    in
    Cmd.info "cache" ~doc ~man
  in
  Cmd.group info [ trim; size ]
