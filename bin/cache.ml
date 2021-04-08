open Stdune
open Import

let name = "cache"

(* CR-someday amokhov: Implement other commands supported by Jenga. *)

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|Dune can share build artifacts between workspaces. Currently, the only
        action supported by this command is `trim`, but we plan to provide more
        functionality soon. |}
  ; `S "ACTIONS"
  ; `P {|$(b,trim) trim the shared cache to free space.|}
  ; `Blocks Common.help_secs
  ]

let doc = "Manage the shared cache of build artifacts"

let info = Term.info name ~doc ~man

let trim ~trimmed_size ~size =
  Log.init_disabled ();
  let open Result.O in
  match
    let+ goal =
      match (trimmed_size, size) with
      | Some trimmed_size, None -> Result.Ok trimmed_size
      | None, Some size ->
        Result.Ok (Int64.sub (Dune_cache.Trimmer.overhead_size ()) size)
      | _ -> Result.Error "specify either --size or --trimmed-size"
    in
    Dune_cache.Trimmer.trim ~goal
  with
  | Error s -> User_error.raise [ Pp.text s ]
  | Ok { trimmed_bytes } ->
    User_message.print
      (User_message.make [ Pp.textf "Freed %Li bytes" trimmed_bytes ])

type mode = Trim

let modes = [ ("trim", Trim) ]

let term =
  Term.ret
  @@ let+ mode =
       Arg.(
         value
         & pos 0 (some (enum modes)) None
         & info [] ~docv:"ACTION"
             ~doc:
               (Printf.sprintf "The cache action to perform (%s)"
                  (Arg.doc_alts_enum modes)))
     and+ trimmed_size =
       Arg.(
         value
         & opt (some bytes) None
         & info ~docv:"BYTES" [ "trimmed-size" ]
             ~doc:"size to trim from the cache")
     and+ size =
       Arg.(
         value
         & opt (some bytes) None
         & info ~docv:"BYTES" [ "size" ] ~doc:"size to trim the cache to")
     in
     match mode with
     | Some Trim -> `Ok (trim ~trimmed_size ~size)
     | None -> `Help (`Pager, Some name)

let command = (term, info)
