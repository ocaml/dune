module Raw = struct
  external abi_version : unit -> int = "dune_landlock_abi_version"
  external write_access_rights : int -> int64 = "dune_landlock_write_access_rights"

  external file_write_access_rights
    :  int
    -> int64
    = "dune_landlock_file_write_access_rights"

  external create_ruleset : int64 -> Unix.file_descr = "dune_landlock_create_ruleset"

  external add_rule
    :  Unix.file_descr
    -> string
    -> int64
    -> unit
    = "dune_landlock_add_rule"

  external restrict_self : Unix.file_descr -> unit = "dune_landlock_restrict_self"
end

let minimum_abi = 3

module Access = struct
  type t =
    { directory : int64
    ; file : int64
    }

  let for_abi abi =
    { directory = Raw.write_access_rights abi; file = Raw.file_write_access_rights abi }
  ;;

  let handled_raw t = t.directory

  let raw_for_file_kind t = function
    | Unix.S_DIR -> t.directory
    | S_REG | S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK -> t.file
  ;;
end

let available () =
  match Raw.abi_version () with
  | abi -> abi >= minimum_abi && not (Int64.equal (Raw.write_access_rights abi) 0L)
  | exception Unix.Unix_error _ -> false
;;

module Ruleset0 = struct
  type t =
    { fd : Fd.t
    ; access : Access.t
    }

  let create_empty () =
    let access = Access.for_abi (Raw.abi_version ()) in
    let fd = Raw.create_ruleset (Access.handled_raw access) in
    (* Close the ruleset in unrelated children that may [execve] while this fd
       is open. The intended child uses and closes it before its own exec. *)
    Unix.set_close_on_exec fd;
    { fd = Fd.unsafe_of_unix_file_descr fd; access }
  ;;

  let add t ~path =
    let path = Path.External.to_string path in
    let allowed_access =
      let { Unix.st_kind; _ } = Unix.stat path in
      Access.raw_for_file_kind t.access st_kind
    in
    Raw.add_rule (Fd.unsafe_to_unix_file_descr t.fd) path allowed_access
  ;;

  let file_descr t = Fd.unsafe_to_unix_file_descr t.fd
  let close t = Fd.close t.fd
  let restrict_self t = Raw.restrict_self (file_descr t)
end

module Path_policy = struct
  type t =
    { denied : Path.External.t list
    ; allowed : Path.External.t list
    }

  let realpath_exn path =
    Unix.realpath (Path.External.to_string path) |> Path.External.of_string
  ;;

  let is_descendant path ~of_ =
    Path.External.equal path of_ || Path.External.is_descendant path ~of_
  ;;

  let is_strict_descendant path ~of_ =
    (not (Path.External.equal path of_)) && is_descendant path ~of_
  ;;

  let resolve path =
    match realpath_exn path with
    | path -> path
    | exception Unix.Unix_error (error, _, _) ->
      User_error.raise
        [ Pp.textf
            "unable to resolve Landlock policy path %s: %s"
            (Path.External.to_string_maybe_quoted path)
            (Unix.error_message error)
        ]
  ;;

  let validate_allowed_path ~denied allowed =
    if not (List.exists denied ~f:(fun deny -> is_strict_descendant allowed ~of_:deny))
    then
      User_error.raise
        [ Pp.textf
            "allowed path %s must be strictly inside a denied path"
            (Path.External.to_string_maybe_quoted allowed)
        ];
    match List.find denied ~f:(fun deny -> is_descendant deny ~of_:allowed) with
    | None -> ()
    | Some deny ->
      User_error.raise
        [ Pp.textf
            "allowed path %s must not cover denied path %s"
            (Path.External.to_string_maybe_quoted allowed)
            (Path.External.to_string_maybe_quoted deny)
        ]
  ;;

  let create ~denied ~allowed =
    if List.is_empty denied
    then Code_error.raise "Landlock.Policy.create: denied paths must not be empty" [];
    let denied = List.map denied ~f:resolve in
    let allowed = List.map allowed ~f:resolve in
    List.iter allowed ~f:(validate_allowed_path ~denied);
    { denied; allowed }
  ;;

  let add_best_effort_rule ruleset path =
    match Ruleset0.add ruleset ~path with
    | () -> ()
    | exception Unix.Unix_error ((ENOENT | EACCES | EPERM | ENOTDIR | ELOOP), _, _) -> ()
  ;;

  let add_required_rule ruleset path =
    match Ruleset0.add ruleset ~path with
    | () -> ()
    | exception Unix.Unix_error (err, _, _) ->
      User_error.raise
        [ Pp.textf
            "failed to add required Landlock rule for %s: %s"
            (Path.External.to_string_maybe_quoted path)
            (Unix.error_message err)
        ]
  ;;

  let stat_key path =
    match Unix.stat (Path.External.to_string path) with
    | { Unix.st_dev; st_ino; _ } -> Some (st_dev, st_ino)
    | exception Unix.Unix_error _ -> None
  ;;

  let is_protected_alias path ~protected_ancestors =
    match stat_key path with
    | None -> false
    | Some key ->
      List.exists protected_ancestors ~f:(Tuple.T2.equal Int.equal Int.equal key)
  ;;

  let path_and_ancestors path =
    let rec loop path acc =
      match Path.External.parent path with
      | None -> path :: acc
      | Some parent -> loop parent (path :: acc)
    in
    loop path []
  ;;

  (* Landlock rules are allow-lists. To keep denied subtrees write-protected,
     allow writes to siblings of every ancestor of every protected path. Skip
     lexical descendants of protected paths, paths whose realpath is at or
     above any protected path, and bind-mount aliases of protected ancestors.
     Explicit allow paths are then added as punch-holes. *)
  let add_rules { denied; allowed } ruleset =
    let skeleton =
      List.concat_map denied ~f:path_and_ancestors |> Path.External.Set.of_list
    in
    let protected_ancestors =
      Path.External.Set.to_list skeleton |> List.filter_map ~f:stat_key
    in
    let in_or_above_protected realpath =
      List.exists denied ~f:(fun path ->
        is_descendant realpath ~of_:path || is_descendant path ~of_:realpath)
    in
    Path.External.Set.iter skeleton ~f:(fun dir ->
      match Sys.readdir (Path.External.to_string dir) with
      | exception Sys_error _ -> ()
      | entries ->
        Array.iter entries ~f:(fun entry ->
          let path = Path.External.relative_fname dir (Filename.of_string_exn entry) in
          if not (Path.External.Set.mem skeleton path)
          then (
            match realpath_exn path with
            | exception Unix.Unix_error _ -> ()
            | realpath ->
              if
                not
                  (in_or_above_protected realpath
                   || is_protected_alias path ~protected_ancestors)
              then add_best_effort_rule ruleset path)));
    List.iter allowed ~f:(add_required_rule ruleset)
  ;;
end

module Policy = struct
  type t = Path_policy.t

  let create ~deny_write ~allow_write =
    Path_policy.create ~denied:deny_write ~allowed:allow_write
  ;;
end

module Ruleset = struct
  include Ruleset0

  let create policy =
    let ruleset = create_empty () in
    match Path_policy.add_rules policy ruleset with
    | () -> ruleset
    | exception exn ->
      close ruleset;
      raise exn
  ;;
end

let restrict_self policy =
  let ruleset = Ruleset.create policy in
  Exn.protect
    ~f:(fun () -> Ruleset.restrict_self ruleset)
    ~finally:(fun () -> Ruleset.close ruleset)
;;
