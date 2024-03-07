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

           \$ dune cache trim --size=1GB |}
      ; `Pre
          {|Trimming 500 MB from the Dune cache.

           \$ dune cache trim --trimmed-size=500MB |}
      ]
    in
    Cmd.info "trim" ~doc ~man
  in
  Cmd.v info
  @@ let+ trimmed_size =
       Arg.(
         value
         & opt (some bytes) None
         & info
             ~docv:"BYTES"
             [ "trimmed-size" ]
             ~doc:"Size to trim from the cache. $(docv) is the same as for --size.")
     and+ size =
       Arg.(
         value
         & opt (some bytes) None
         & info
             ~docv:"BYTES"
             [ "size" ]
             ~doc:
               (sprintf
                  "Size to trim the cache to. $(docv) is the number of bytes followed by \
                   a unit. Byte units can be one of %s."
                  (String.enumerate_or
                     (List.map
                        ~f:(fun (units, _) -> List.hd units)
                        Bytes_unit.conversion_table))))
     in
     Log.init_disabled ();
     let open Result.O in
     match
       let+ goal =
         match trimmed_size, size with
         | Some trimmed_size, None -> Result.Ok trimmed_size
         | None, Some size ->
           Result.Ok (Int64.sub (Dune_cache.Trimmer.overhead_size ()) size)
         | _ -> Result.Error "please specify either --size or --trimmed-size"
       in
       Dune_cache.Trimmer.trim ~goal
     with
     | Error s -> User_error.raise [ Pp.text s ]
     | Ok { trimmed_bytes; number_of_files_removed } ->
       User_message.print
         (User_message.make
            [ Pp.textf
                "Freed %s (%d files removed)"
                (Bytes_unit.pp trimmed_bytes)
                number_of_files_removed
            ])
;;

let size =
  let info =
    let doc = "Query the size of the Dune cache." in
    let man =
      [ `P
          "Compute the total size of files in the Dune cache which are not hardlinked \
           from any build directory and output it in a human-readable form."
      ]
    in
    Cmd.info "size" ~doc ~man
  in
  Cmd.v info
  @@ let+ machine_readable =
       Arg.(
         value
         & flag
         & info [ "machine-readable" ] ~doc:"Outputs size as a plain number of bytes.")
     in
     let size = Dune_cache.Trimmer.overhead_size () in
     if machine_readable
     then User_message.print (User_message.make [ Pp.textf "%Ld" size ])
     else User_message.print (User_message.make [ Pp.textf "%s" (Bytes_unit.pp size) ])
;;

let clear =
  let info =
    let doc = "Clear the Dune cache." in
    let man = [ `P "Remove any traces of the Dune cache." ] in
    Cmd.info "clear" ~doc ~man
  in
  Cmd.v info @@ Term.(const Dune_cache_storage.clear $ const ())
;;

let command =
  let info =
    let doc = "Manage Dune's shared cache of build artifacts." in
    let man =
      [ `S "DESCRIPTION"
      ; `P
          "Dune can share build artifacts between workspaces. We currently only support \
           a few subcommands; however, we plan to provide more functionality soon."
      ]
    in
    Cmd.info "cache" ~doc ~man
  in
  Cmd.group info [ trim; size; clear ]
;;
