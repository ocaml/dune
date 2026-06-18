open Import

type command =
  { prog : string
  ; argv : string list
  }

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

let stat_key path =
  match Unix.stat path with
  | { Unix.st_dev; st_ino; _ } -> Some (st_dev, st_ino)
  | exception Unix.Unix_error _ -> None
;;

let add_write_rule ruleset_fd ~write_access ~file_write_access path =
  let access =
    match Unix.stat path with
    | { Unix.st_kind = S_DIR; _ } -> write_access
    | _ -> file_write_access
    | exception Unix.Unix_error _ -> 0L
  in
  add_rule ruleset_fd path access
;;

let is_descendant path ~of_ =
  String.equal path of_
  || ((not (String.equal of_ Filename.dir_sep))
      && Option.is_some (String.drop_prefix path ~prefix:(of_ ^ Filename.dir_sep)))
;;

let is_protected_alias path ~protected_ancestors =
  match stat_key path with
  | None -> false
  | Some key ->
    List.exists protected_ancestors ~f:(Tuple.T2.equal Int.equal Int.equal key)
;;

let add_writable_siblings
      ruleset_fd
      ~write_access
      ~file_write_access
      ~protected_path
      ~protected_ancestors
      ~dir
      ~except
  =
  match Sys.readdir dir with
  | exception Sys_error _ -> ()
  | entries ->
    Array.iter
      ~f:(fun entry ->
        if not (String.equal entry except)
        then (
          let path = concat_dir dir entry in
          let realpath = normalize_absolute_filename path in
          if
            (not (is_descendant protected_path ~of_:realpath))
            && (not (is_descendant realpath ~of_:protected_path))
            && not (is_protected_alias path ~protected_ancestors)
          then add_write_rule ruleset_fd ~write_access ~file_write_access path))
      entries
;;

let path_and_ancestors path =
  let rec loop dir acc = function
    | [] -> List.rev (dir :: acc)
    | next :: rest -> loop (concat_dir dir next) (dir :: acc) rest
  in
  loop Filename.dir_sep [] (split_absolute path)
;;

(* Landlock rules are allow-lists. To keep the cache read-only while leaving
   the rest of the filesystem writable, allow writes to siblings of every
   protected path ancestor. Skip lexical descendants and bind-mount aliases of
   the protected ancestors, otherwise an alternate path such as [/tmp] could
   re-grant access to the cache. *)
let add_rules ruleset_fd ~write_access ~file_write_access protected_path =
  let components = split_absolute protected_path in
  let protected_ancestors =
    path_and_ancestors protected_path |> List.filter_map ~f:stat_key
  in
  let rec loop dir = function
    | [] -> ()
    | next :: rest ->
      add_writable_siblings
        ruleset_fd
        ~write_access
        ~file_write_access
        ~protected_path
        ~protected_ancestors
        ~dir
        ~except:next;
      loop (concat_dir dir next) rest
  in
  loop Filename.dir_sep components
;;

let with_ruleset ~handled_access ~f =
  let fd = Raw.create_ruleset handled_access in
  Exn.protect ~f:(fun () -> f fd) ~finally:(fun () -> Fd.close (Fd.unsafe_of_int fd))
;;

let restrict_path_to_read_only path =
  let abi = abi_version () in
  if abi < minimum_abi
  then User_error.raise [ Pp.text "Landlock is not available on this system" ];
  let write_access = Raw.write_access_rights abi in
  if Int64.equal write_access 0L
  then User_error.raise [ Pp.text "Landlock write restrictions are not available" ];
  let file_write_access = Raw.file_write_access_rights abi in
  let protected_path = normalize_absolute_filename path in
  with_ruleset ~handled_access:write_access ~f:(fun ruleset_fd ->
    add_rules ruleset_fd ~write_access ~file_write_access protected_path;
    Raw.restrict_self ruleset_fd)
;;

let restrict_shared_cache_to_read_only () =
  let build_cache_dir = Lazy.force Dune_cache.Layout.build_cache_dir in
  Path.mkdir_p build_cache_dir;
  restrict_path_to_read_only (Path.to_string build_cache_dir)
;;

let wrap ~dune_prog argv =
  if available ()
  then (
    let prog = Path.to_string dune_prog in
    Some { prog; argv = [ prog; "internal"; "with-landlock"; "--" ] @ argv })
  else None
;;

let wrap_exn ~dune_prog argv =
  match wrap ~dune_prog argv with
  | Some command -> command
  | None -> User_error.raise [ Pp.text "Landlock is not available on this system" ]
;;

module With_landlock = struct
  let command =
    let doc = "Run a command under Dune's Landlock wrapper." in
    let info = Cmd.info "with-landlock" ~doc in
    let term =
      let+ argv = Arg.(value & pos_all string [] (info [] ~docv:"COMMAND" ~doc:None)) in
      match argv with
      | [] -> User_error.raise [ Pp.text "missing command after --" ]
      | prog :: args ->
        restrict_shared_cache_to_read_only ();
        Proc.restore_cwd_and_execve (Util.resolve_prog prog) args ~env:Env.initial
    in
    Cmd.v info term
  ;;
end
