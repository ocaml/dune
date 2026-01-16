open Import
open Fiber.O

let action_trace_root =
  lazy
    (let root = Path.Build.(relative root ".action-trace") in
     Path.mkdir_p (Path.build root);
     root)
;;

type t =
  { dir : Path.Build.t
  ; digest : string
  }

let add_to_env t env =
  Env.add
    env
    ~var:Dune_action_trace.Private.trace_dir_env_var
    ~value:(Path.to_absolute_filename (Path.build t.dir))
;;

let create digest =
  let root = Lazy.force action_trace_root in
  let digest = Dune_digest.to_string digest in
  { dir = Path.Build.relative root digest; digest }
;;

let collect_file file ~digest =
  Io.with_file_in ~binary:true (Path.build file) ~f:(fun chan ->
    let rec loop () =
      (* CR-soon rgrinberg: handle json payloads as well *)
      match Csexp.input_opt chan with
      | Ok None -> ()
      | Error _ ->
        User_error.raise
          [ Pp.textf "invalid action trace in %s" (Path.Build.to_string_maybe_quoted file)
          ]
      | Ok (Some s) ->
        Dune_trace.emit ~buffered:true Action (fun () ->
          Dune_trace.Event.Action.trace ~digest s);
        loop ()
    in
    loop ())
;;

let collect { dir; digest } =
  let* () = Fiber.return () in
  let unrecognized = Queue.create () in
  let errors = Queue.create () in
  let needs_cleanup = ref false in
  let rec loop dir =
    match Path.Untracked.readdir_unsorted_with_kinds (Path.build dir) with
    | Error (ENOENT, _, _) -> ()
    | Error e ->
      needs_cleanup := true;
      let error =
        User_error.make
          [ Pp.textf "unreadable dir %s" (Path.Build.to_string_maybe_quoted dir)
          ; Unix_error.Detailed.pp e
          ]
      in
      Queue.push errors (User_error.E error)
    | Ok names ->
      needs_cleanup := true;
      let dirs, files =
        List.filter_partition_map names ~f:(fun (name, (kind : Unix.file_kind)) ->
          let kind =
            match kind with
            | S_LNK ->
              (match Path.Untracked.stat (Path.build (Path.Build.relative dir name)) with
               | Ok s -> Ok s.st_kind
               | Error e -> Error e)
            | _ -> Ok kind
          in
          match kind with
          | Ok S_REG -> Right name
          | Ok S_DIR -> Left name
          | Ok _ ->
            Queue.push unrecognized (Path.Build.relative dir name);
            Skip
          | Error error ->
            let error =
              User_message.make
                [ Pp.textf
                    "broken symlink %s"
                    (Path.Build.to_string_maybe_quoted (Path.Build.relative dir name))
                ; Unix_error.Detailed.pp error
                ]
            in
            Queue.push errors (User_error.E error);
            Skip)
        (* CR-someday rgrinberg: handle symlinks? *)
      in
      List.iter files ~f:(fun file ->
        try collect_file (Path.Build.relative dir file) ~digest with
        | exn -> Queue.push errors exn);
      List.iter dirs ~f:(fun name -> loop (Path.Build.relative dir name))
  in
  loop dir;
  Dune_trace.flush ();
  if !needs_cleanup then Fpath.rm_rf (Path.to_string (Path.build dir));
  (match Queue.to_list unrecognized with
   | [] -> ()
   | unrecognized ->
     let error =
       User_message.make
         [ Pp.text "unrecognized files"
         ; Pp.enumerate unrecognized ~f:(fun f ->
             Pp.verbatim (Path.Build.to_string_maybe_quoted f))
         ]
     in
     Queue.push errors (User_error.E error));
  match Queue.to_list errors with
  | [] -> Fiber.return ()
  | errors ->
    let errors =
      (* A bit of a useless callstack, but good enough for our purposes here. *)
      let backtrace = Printexc.get_callstack 10 in
      List.map errors ~f:(fun exn -> { Exn_with_backtrace.exn; backtrace })
    in
    Fiber.reraise_all errors
;;
