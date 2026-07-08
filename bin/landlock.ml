open Import

module Raw = struct
  external abi_version : unit -> int = "dune_landlock_abi_version"
  external write_access_rights : int -> int64 = "dune_landlock_write_access_rights"

  external file_write_access_rights
    :  int
    -> int64
    = "dune_landlock_file_write_access_rights"

  external create_ruleset : int64 -> int = "dune_landlock_create_ruleset"
  external add_rule : int -> string -> int64 -> unit = "dune_landlock_add_rule"
  external restrict_self : int -> unit = "dune_landlock_restrict_self"
end

let abi_version () = Raw.abi_version ()

(* ABI 2 adds LANDLOCK_ACCESS_FS_REFER, which is needed to prevent moving files
   out of a denied tree. ABI 3 adds LANDLOCK_ACCESS_FS_TRUNCATE, which is needed
   to prevent emptying denied files through truncate. *)
let minimum_abi = 3

let available () =
  match abi_version () with
  | n -> n >= minimum_abi && not (Int64.equal (Raw.write_access_rights n) 0L)
  | exception Unix.Unix_error _ -> false
;;

let normalize_absolute_filename path =
  let path =
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path
  in
  match Unix.realpath path with
  | path -> path
  | exception Unix.Unix_error _ -> path
;;

let split_absolute path =
  let rec loop path acc =
    let dir = Filename.dirname path in
    let base = Filename.basename path in
    if String.equal path dir then acc else loop dir (base :: acc)
  in
  loop path []
;;

let concat_dir dir name =
  if String.equal dir Filename.dir_sep
  then Filename.concat Filename.dir_sep name
  else Filename.concat dir name
;;

let add_rule ruleset_fd path access =
  if not (Int64.equal access 0L)
  then (
    match Raw.add_rule ruleset_fd path access with
    | () -> ()
    | exception Unix.Unix_error ((ENOENT | EACCES | EPERM | ENOTDIR | ELOOP), _, _) -> ())
;;

let add_rule_exn ruleset_fd path access =
  if not (Int64.equal access 0L)
  then (
    match Raw.add_rule ruleset_fd path access with
    | () -> ()
    | exception Unix.Unix_error (err, _, _) ->
      User_error.raise
        [ Pp.textf
            "failed to allow writes to %s: %s"
            (String.maybe_quoted path)
            (Unix.error_message err)
        ])
;;

let stat_key path =
  match Unix.stat path with
  | { Unix.st_dev; st_ino; _ } -> Some (st_dev, st_ino)
  | exception Unix.Unix_error _ -> None
;;

let write_access_for_path ~write_access ~file_write_access path =
  match Unix.stat path with
  | { Unix.st_kind = S_DIR; _ } -> write_access
  | _ -> file_write_access
;;

let add_write_rule ruleset_fd ~write_access ~file_write_access path =
  let access =
    match write_access_for_path ~write_access ~file_write_access path with
    | access -> access
    | exception Unix.Unix_error _ -> 0L
  in
  add_rule ruleset_fd path access
;;

let add_allow_write_rule ruleset_fd ~write_access ~file_write_access path =
  let access =
    match write_access_for_path ~write_access ~file_write_access path with
    | access -> access
    | exception Unix.Unix_error (err, _, _) ->
      User_error.raise
        [ Pp.textf
            "failed to allow writes to %s: %s"
            (String.maybe_quoted path)
            (Unix.error_message err)
        ]
  in
  add_rule_exn ruleset_fd path access
;;

let is_descendant path ~of_ =
  String.equal path of_
  || String.equal of_ Filename.dir_sep
  || ((not (String.equal of_ Filename.dir_sep))
      && Option.is_some (String.drop_prefix path ~prefix:(of_ ^ Filename.dir_sep)))
;;

let is_strict_descendant path ~of_ =
  (not (String.equal path of_)) && is_descendant path ~of_
;;

let is_protected_alias path ~protected_ancestors =
  match stat_key path with
  | None -> false
  | Some key ->
    List.exists protected_ancestors ~f:(Tuple.T2.equal Int.equal Int.equal key)
;;

let path_and_ancestors path =
  let rec loop dir acc = function
    | [] -> List.rev (dir :: acc)
    | next :: rest -> loop (concat_dir dir next) (dir :: acc) rest
  in
  loop Filename.dir_sep [] (split_absolute path)
;;

(* Landlock rules are allow-lists. To keep a set of subtrees write-protected,
   allow writes to siblings of every ancestor of every protected path. Skip
   lexical descendants of protected paths, paths whose realpath is at or above
   any protected path (symlink aliases), and bind-mount aliases of
   protected-path ancestors. Then apply explicit allow paths as punch-holes on
   top. Ancestor directories of denied paths are intentionally not granted
   directory write rights, so creating/removing entries directly in those
   ancestors is also denied. *)
let add_rules ruleset_fd ~write_access ~file_write_access ~deny_paths ~allow_paths =
  (match deny_paths with
   | [] -> ()
   | _ ->
     let skeleton =
       List.concat_map deny_paths ~f:path_and_ancestors |> String.Set.of_list
     in
     let protected_ancestors =
       String.Set.to_list skeleton |> List.filter_map ~f:stat_key
     in
     let in_or_above_protected realpath =
       List.exists deny_paths ~f:(fun p ->
         is_descendant realpath ~of_:p || is_descendant p ~of_:realpath)
     in
     String.Set.iter skeleton ~f:(fun dir ->
       match Sys.readdir dir with
       | exception Sys_error _ -> ()
       | entries ->
         Array.iter entries ~f:(fun entry ->
           let path = concat_dir dir entry in
           if not (String.Set.mem skeleton path)
           then (
             let realpath = normalize_absolute_filename path in
             if
               not
                 (in_or_above_protected realpath
                  || is_protected_alias path ~protected_ancestors)
             then add_write_rule ruleset_fd ~write_access ~file_write_access path))));
  List.iter
    allow_paths
    ~f:(add_allow_write_rule ruleset_fd ~write_access ~file_write_access)
;;

let with_ruleset ~handled_access ~f =
  let fd = Raw.create_ruleset handled_access in
  Exn.protect ~f:(fun () -> f fd) ~finally:(fun () -> Fd.close (Fd.unsafe_of_int fd))
;;

let validate_allow_paths ~deny_write ~allow_write =
  List.iter allow_write ~f:(fun allow ->
    if not (List.exists deny_write ~f:(fun deny -> is_strict_descendant allow ~of_:deny))
    then
      User_error.raise
        [ Pp.textf
            "--allow-write path %s must be strictly inside a --deny-write path"
            (String.maybe_quoted allow)
        ];
    match List.find deny_write ~f:(fun deny -> is_descendant deny ~of_:allow) with
    | None -> ()
    | Some deny ->
      User_error.raise
        [ Pp.textf
            "--allow-write path %s must not cover --deny-write path %s"
            (String.maybe_quoted allow)
            (String.maybe_quoted deny)
        ])
;;

(* Contract for [restrict_write_policy]:

   - Paths are normalized before restriction. Relative paths are interpreted
     relative to the wrapper's current working directory. Existing paths are
     resolved with [realpath], so a symlink argument protects its target.
     Nonexistent paths fall back to lexical normalization.
   - With no [deny_write] paths, the policy is a no-op, including any
     [allow_write] paths.
   - With [deny_write = ["/"]], the policy is a pure allow-list: only explicit
     allow paths are writable.
   - Otherwise, writes are denied in each denied subtree and directly in each
     ancestor directory of a denied path. Existing siblings of protected
     ancestors keep write-in-place rights, but creating/removing entries in
     those ancestor directories is denied.
   - Allow paths are punch-holes and must be strictly inside a denied path. An
     allow path that covers a deny path is rejected instead of silently
     nullifying the deny.
   - Allow paths must exist when the Landlock rules are installed. Missing or
     inaccessible allow paths are rejected.
   - A file allow path grants write/truncate of that file, not unlink/rename of
     the file. Those operations are controlled by the parent directory's rights.
     A directory allow path grants write operations within that directory, not
     unlink/rename of the directory itself.
   - "Write" here means the Landlock file-system rights handled by
     [write_access_rights]: open-for-write, truncate, create/remove, and
     rename/link. Landlock ABI 3 does not cover chmod/chown/utimes/xattrs,
     writes through already-open file descriptors, writes through hardlinks
     outside the denied tree, or device ioctls. *)
let restrict_write_policy ~deny_write ~allow_write =
  match deny_write with
  | [] -> ()
  | _ ->
    let normalize = List.map ~f:normalize_absolute_filename in
    let deny_write = normalize deny_write in
    let allow_write = normalize allow_write in
    validate_allow_paths ~deny_write ~allow_write;
    let abi =
      match abi_version () with
      | abi -> abi
      | exception Unix.Unix_error _ ->
        User_error.raise [ Pp.text "Landlock is not available on this system" ]
    in
    if abi < minimum_abi
    then User_error.raise [ Pp.text "Landlock is not available on this system" ];
    let write_access = Raw.write_access_rights abi in
    if Int64.equal write_access 0L
    then User_error.raise [ Pp.text "Landlock write restrictions are not available" ];
    let file_write_access = Raw.file_write_access_rights abi in
    with_ruleset ~handled_access:write_access ~f:(fun ruleset_fd ->
      add_rules
        ruleset_fd
        ~write_access
        ~file_write_access
        ~deny_paths:deny_write
        ~allow_paths:allow_write;
      Raw.restrict_self ruleset_fd)
;;

module With_landlock = struct
  external sys_exit : int -> 'a = "caml_sys_exit"

  let execve prog args ~env =
    let argv = Array.of_list (prog :: args) in
    let env = Env.to_unix env |> Array.of_list in
    Stdlib.do_at_exit ();
    if Sys.win32 || Platform.OS.value = Platform.OS.Haiku
    then (
      let pid =
        Unix.create_process_env prog argv env Unix.stdin Unix.stdout Unix.stderr
      in
      match snd (Unix.waitpid [] pid) with
      | WEXITED n -> sys_exit n
      | WSIGNALED _ -> sys_exit 255
      | WSTOPPED _ -> assert false)
    else (
      ignore (Unix.sigprocmask SIG_SETMASK [] : int list);
      Unix.execve prog argv env)
  ;;

  let command =
    let doc = "Run a command under Dune's Landlock wrapper." in
    let info = Cmd.info "with-landlock" ~doc in
    let path_list_flag ~name ~doc =
      Arg.(value & opt_all string [] & info [ name ] ~docv:"PATH" ~doc:(Some doc))
    in
    let term =
      let+ deny_write =
        path_list_flag
          ~name:"deny-write"
          ~doc:
            "Forbid writes within this subtree and directly in its ancestor directories. \
             With PATH=/, switch to a pure allow-list. Repeatable."
      and+ allow_write =
        path_list_flag
          ~name:"allow-write"
          ~doc:
            "Allow writes to a path strictly inside a --deny-write subtree. Has no \
             effect unless --deny-write is also passed. The path must exist when the \
             policy is installed. Repeatable."
      and+ argv = Arg.(value & pos_all string [] (info [] ~docv:"COMMAND" ~doc:None)) in
      match argv with
      | [] -> User_error.raise [ Pp.text "missing command after --" ]
      | prog :: args ->
        restrict_write_policy ~deny_write ~allow_write;
        execve (Util.resolve_prog prog) args ~env:Env.initial
    in
    Cmd.v info term
  ;;
end
