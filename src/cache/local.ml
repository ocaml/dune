open Stdune
open Result.O
open Cache_intf

type t =
  { root : Path.t
  ; build_root : Path.t option
  ; info : User_message.Style.t Pp.t list -> unit
  ; warn : User_message.Style.t Pp.t list -> unit
  ; repositories : repository list
  ; handler : handler
  ; duplication_mode : Duplication_mode.t
  ; temp_dir : Path.t
  }

module Trimming_result = struct
  type t =
    { trimmed_files_size : int
    ; trimmed_files : Path.t list
    ; trimmed_metafiles : Path.t list
    }

  let empty =
    { trimmed_files_size = 0; trimmed_files = []; trimmed_metafiles = [] }

  let add t ~size ~file =
    { t with
      trimmed_files = file :: t.trimmed_files
    ; trimmed_files_size = size + t.trimmed_files_size
    }
end

let default_root () =
  Path.L.relative (Path.of_string Xdg.cache_dir) [ "dune"; "db"; "v2" ]

(* How to handle collisions. E.g. another version could assume collisions are
   not possible *)
module Collision = struct
  type res =
    | Found of Path.t
    | Not_found of Path.t

  (* We need to ensure we do not create holes in the suffix numbering for this
     to work *)
  let search path file =
    let rec loop n =
      let path = Path.extend_basename path ~suffix:("." ^ string_of_int n) in
      if Path.exists path then
        if Io.compare_files path file = Ordering.Eq then
          Found path
        else
          loop (n + 1)
      else
        Not_found path
    in
    loop 1
end

module type FSScheme = sig
  val path : Path.t -> Digest.t -> Path.t

  val digest : Path.t -> Digest.t

  val list : Path.t -> Path.t list
end

(* Where to store file with a given hash. In this case ab/abcdef. *)
module FirstTwoCharsSubdir : FSScheme = struct
  let path root hash =
    let hash = Digest.to_string hash in
    let short_hash = String.sub hash ~pos:0 ~len:2 in
    Path.L.relative root [ short_hash; hash ]

  let digest path =
    match Digest.from_hex (Path.basename (fst (Path.split_extension path))) with
    | Some digest -> digest
    | None ->
      Code_error.raise "strange cached file path (not a valid hash)"
        [ (Path.to_string path, Path.to_dyn path) ]

  let list root =
    let f dir =
      let is_hex_char c =
        let char_in s e = Char.compare c s >= 0 && Char.compare c e <= 0 in
        char_in 'a' 'f' || char_in '0' '9'
      and root = Path.L.relative root [ dir ] in
      if String.for_all ~f:is_hex_char dir then
        Array.map ~f:(Path.relative root) (Sys.readdir (Path.to_string root))
      else
        Array.of_list []
    in
    Array.to_list
      (Array.concat
         (Array.to_list (Array.map ~f (Sys.readdir (Path.to_string root)))))
end

module FSSchemeImpl = FirstTwoCharsSubdir

module Metadata_file = struct
  type t =
    { metadata : Sexp.t list
    ; files : File.t list
    }

  let to_sexp { metadata; files } =
    let open Sexp in
    let f ({ in_the_build_directory; in_the_cache; _ } : File.t) =
      Sexp.List
        [ Sexp.Atom
            (Path.Local.to_string (Path.Build.local in_the_build_directory))
        ; Sexp.Atom (Path.to_string in_the_cache)
        ]
    in
    List
      [ List (Atom "metadata" :: metadata)
      ; List (Atom "files" :: List.map ~f files)
      ]

  let of_sexp = function
    | Sexp.List
        [ List (Atom "metadata" :: metadata); List (Atom "files" :: produced) ]
      ->
      let+ files =
        Result.List.map produced ~f:(function
          | List [ Atom in_the_build_directory; Atom in_the_cache ] ->
            let in_the_build_directory =
              Path.Build.of_string in_the_build_directory
            and in_the_cache = Path.of_string in_the_cache in
            Ok
              { File.in_the_cache
              ; in_the_build_directory
              ; digest = FSSchemeImpl.digest in_the_cache
              }
          | _ -> Error "invalid metadata scheme in produced files list")
      in
      { metadata; files }
    | _ -> Error "invalid metadata"

  let of_string s = Csexp.parse (Stream.of_string s) >>= of_sexp

  let to_string f = to_sexp f |> Csexp.to_string

  let parse path =
    Io.with_file_in path ~f:(fun input -> Csexp.parse (Stream.of_channel input))
    >>= of_sexp
end

let root_data cache = Path.relative cache.root "files"

let root_metadata cache = Path.relative cache.root "meta"

let path_metadata cache key = FSSchemeImpl.path (root_metadata cache) key

let path_data cache key = FSSchemeImpl.path (root_data cache) key

let make_path cache path =
  match cache.build_root with
  | Some p -> Result.ok (Path.append_local p path)
  | None ->
    Result.Error
      (Format.asprintf "relative path \"%a\" while no build root was set"
         Path.Local.pp path)

let search cache hash file = Collision.search (path_data cache hash) file

let with_repositories cache repositories = { cache with repositories }

let duplicate ?(duplication = None) cache =
  match Option.value ~default:cache.duplication_mode duplication with
  | Copy -> fun src dst -> Io.copy_file ~src ~dst ()
  | Hardlink -> Path.link

let retrieve cache (file : File.t) =
  let path = Path.build file.in_the_build_directory in
  cache.info
    [ Pp.textf "retrieve %s from cache" (Path.to_string_maybe_quoted path) ];
  duplicate cache file.in_the_cache path;
  path

let deduplicate cache (file : File.t) =
  match cache.duplication_mode with
  | Copy -> ()
  | Hardlink -> (
    let target = Path.Build.to_string file.in_the_build_directory in
    let tmpname = Path.Build.to_string (Path.Build.of_string ".dedup") in
    cache.info
      [ Pp.textf "deduplicate %s from %s" target
          (Path.to_string file.in_the_cache)
      ];
    let rm p = try Unix.unlink p with _ -> () in
    try
      rm tmpname;
      Unix.link (Path.to_string file.in_the_cache) tmpname;
      Unix.rename tmpname target
    with Unix.Unix_error (e, syscall, _) ->
      rm tmpname;
      cache.warn
        [ Pp.textf "error handling dune-cache command: %s: %s" syscall
            (Unix.error_message e)
        ] )

let file_of_promotion = function
  | Already_promoted f
  | Promoted f ->
    f

let apply ~f o v =
  match o with
  | Some o -> f v o
  | None -> v

let promote_sync cache paths key metadata ~repository ~duplication =
  let open Result.O in
  let* repo =
    match repository with
    | Some idx -> (
      match List.nth cache.repositories idx with
      | None -> Result.Error (Printf.sprintf "repository out of range: %i" idx)
      | repo -> Result.Ok repo )
    | None -> Result.Ok None
  in
  let metadata =
    apply
      ~f:(fun metadata repository ->
        metadata
        @ [ Sexp.List [ Sexp.Atom "repo"; Sexp.Atom repository.remote ]
          ; Sexp.List [ Sexp.Atom "commit_id"; Sexp.Atom repository.commit ]
          ])
      repo metadata
  in
  let promote (path, expected_hash) =
    let* abs_path = make_path cache (Path.Build.local path) in
    cache.info [ Pp.textf "promote %s" (Path.to_string abs_path) ];
    let stat = Unix.lstat (Path.to_string abs_path) in
    let* stat =
      if stat.st_kind = S_REG then
        Result.Ok stat
      else
        Result.Error
          (Format.sprintf "invalid file type: %s"
             (Path.string_of_file_kind stat.st_kind))
    in
    let prepare path =
      let dest = Path.relative cache.temp_dir "data" in
      if Path.exists dest then Path.unlink dest;
      duplicate ~duplication cache path dest;
      dest
    in
    let tmp = prepare abs_path in
    let effective_hash = Digest.file_with_stats tmp (Path.stat tmp) in
    if Digest.compare effective_hash expected_hash != Ordering.Eq then (
      let message =
        Printf.sprintf "hash mismatch: %s != %s"
          (Digest.to_string effective_hash)
          (Digest.to_string expected_hash)
      in
      cache.info [ Pp.text message ];
      Result.Error message
    ) else
      match search cache effective_hash tmp with
      | Collision.Found in_the_cache ->
        Path.unlink tmp;
        Path.touch in_the_cache;
        Result.Ok
          (Already_promoted
             { in_the_build_directory = path
             ; in_the_cache
             ; digest = effective_hash
             })
      | Collision.Not_found in_the_cache ->
        Path.mkdir_p (Path.parent_exn in_the_cache);
        let dest = Path.to_string in_the_cache in
        Unix.rename (Path.to_string tmp) dest;
        (* Remove write permissions *)
        Unix.chmod dest (stat.st_perm land 0o555);
        Result.Ok
          (Promoted
             { in_the_build_directory = path
             ; in_the_cache
             ; digest = effective_hash
             })
  in
  let+ promoted = Result.List.map ~f:promote paths in
  let metadata_path = path_metadata cache key
  and metadata_tmp_path = Path.relative cache.temp_dir "metadata"
  and files = List.map ~f:file_of_promotion promoted in
  let metadata_file : Metadata_file.t = { metadata; files } in
  let metadata = Csexp.to_string (Metadata_file.to_sexp metadata_file) in
  Io.write_file metadata_tmp_path metadata;
  let () =
    match Io.read_file metadata_path with
    | contents ->
      if contents <> metadata then
        User_warning.emit
          [ Pp.textf "non reproductible collision on rule %s"
              (Digest.to_string key)
          ]
    | exception Sys_error _ -> Path.mkdir_p (Path.parent_exn metadata_path)
  in
  Path.rename metadata_tmp_path metadata_path;
  let f = function
    | Already_promoted file when cache.duplication_mode <> Copy ->
      cache.handler (Dedup file)
    | _ -> ()
  in
  List.iter ~f promoted;
  (metadata_file, promoted)

let promote cache paths key metadata ~repository ~duplication =
  Result.map ~f:ignore
    (promote_sync cache paths key metadata ~repository ~duplication)

let search cache key =
  let path = path_metadata cache key in
  let* sexp =
    try
      Io.with_file_in path ~f:(fun input ->
          Csexp.parse (Stream.of_channel input))
    with Sys_error _ -> Error "no cached file"
  in
  let+ metadata = Metadata_file.of_sexp sexp in
  (* Touch cache files so they are removed last by LRU trimming *)
  let () =
    let f (file : File.t) =
      (* There is no point in trying to trim out files that are missing : dune
         will have to check when hardlinking anyway since they could disappear
         inbetween. *)
      try Path.touch file.in_the_cache
      with Unix.(Unix_error (ENOENT, _, _)) -> ()
    in
    List.iter ~f metadata.files
  in
  (metadata.metadata, metadata.files)

let set_build_dir cache p = { cache with build_root = Some p }

let teardown cache = Path.rm_rf ~allow_external:true cache.temp_dir

let detect_duplication_mode root =
  let () = Path.mkdir_p root in
  let beacon = Path.relative root "beacon"
  and target = Path.relative Path.build_dir ".cache-beacon" in
  let () = Path.touch beacon in
  let rec test () =
    match Path.link beacon target with
    | exception Unix.Unix_error (Unix.EEXIST, _, _) ->
      Path.unlink_no_err target;
      test ()
    | exception Unix.Unix_error _ -> Duplication_mode.Copy
    | () -> Duplication_mode.Hardlink
  in
  test ()

let make ?(root = default_root ())
    ?(duplication_mode = detect_duplication_mode root)
    ?(log = Dune_util.Log.info) ?(warn = fun pp -> User_warning.emit pp) handler
    =
  if Path.basename root <> "v2" then
    Result.Error "unable to read dune-cache"
  else
    let res =
      { root
      ; build_root = None
      ; info = log
      ; warn
      ; repositories = []
      ; handler
      ; duplication_mode
      ; temp_dir =
          Path.temp_dir ~temp_dir:root "promoting"
            (string_of_int (Unix.getpid ()))
      }
    in
    Path.mkdir_p @@ root_metadata res;
    Path.mkdir_p @@ root_data res;
    Result.ok res

let duplication_mode cache = cache.duplication_mode

let trimmable stats = stats.Unix.st_nlink = 1

let _garbage_collect default_trim cache =
  let path = root_metadata cache in
  let metas =
    List.map ~f:(fun p -> (p, Metadata_file.parse p)) (FSSchemeImpl.list path)
  in
  let f default_trim = function
    | p, Result.Error msg ->
      cache.warn
        [ Pp.textf "remove invalid metadata file %s: %s"
            (Path.to_string_maybe_quoted p)
            msg
        ];
      Path.unlink_no_err p;
      { default_trim with Trimming_result.trimmed_metafiles = [ p ] }
    | p, Result.Ok { Metadata_file.files; _ } ->
      if
        List.for_all
          ~f:(fun { File.in_the_cache; _ } -> Path.exists in_the_cache)
          files
      then
        default_trim
      else (
        cache.info
          [ Pp.textf
              "remove metadata file %s as some produced files are missing"
              (Path.to_string_maybe_quoted p)
          ];
        let res =
          List.fold_left ~init:default_trim
            ~f:(fun trim f ->
              let p = f.File.in_the_cache in
              try
                let stats = Path.stat p in
                if trimmable stats then (
                  Path.unlink_no_err p;
                  Trimming_result.add trim ~file:p ~size:stats.st_size
                ) else
                  trim
              with Unix.Unix_error (Unix.ENOENT, _, _) -> trim)
            files
        in
        Path.unlink_no_err p;
        res
      )
  in
  List.fold_left ~init:default_trim ~f metas

let garbage_collect = _garbage_collect Trimming_result.empty

let trim cache free =
  let path = root_data cache in
  let files = FSSchemeImpl.list path in
  let f path =
    let stats = Path.stat path in
    if trimmable stats then
      Some (path, stats.st_size, stats.st_mtime)
    else
      None
  and compare (_, _, t1) (_, _, t2) = Ordering.of_int (Stdlib.compare t1 t2) in
  let files = List.sort ~compare (List.filter_map ~f files)
  and delete (trim : Trimming_result.t) (path, size, _) =
    if trim.trimmed_files_size >= free then
      trim
    else (
      Path.unlink path;
      Trimming_result.add trim ~size ~file:path
    )
  in
  let trim = List.fold_left ~init:Trimming_result.empty ~f:delete files in
  _garbage_collect trim cache

let size cache =
  let root = root_data cache in
  let files = FSSchemeImpl.list root in
  let stats =
    let f p =
      try
        let stats = Path.stat p in
        if trimmable stats then
          stats.st_size
        else
          0
      with Unix.Unix_error (Unix.ENOENT, _, _) -> 0
    in
    List.map ~f files
  in
  List.fold_left ~f:(fun acc size -> acc + size) ~init:0 stats
