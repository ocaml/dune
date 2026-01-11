open Import

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
      match Csexp.input chan with
      | Error _ ->
        (* CR-soon rgrinberg: error handling *)
        ()
      | Ok s ->
        Dune_trace.emit Action (fun () -> Dune_trace.Event.Action.trace ~digest s);
        loop ()
    in
    loop ())
;;

let collect { dir; digest } =
  match Path.Untracked.readdir_unsorted_with_kinds (Path.build dir) with
  | Error _ -> ()
  | Ok files ->
    List.iter files ~f:(fun (file, kind) ->
      match (kind : Unix.file_kind) with
      | S_REG -> collect_file (Path.Build.relative dir file) ~digest
      | _ ->
        (* CR-soon rgrinberg: error handling *)
        ())
;;
