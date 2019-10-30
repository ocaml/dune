open Stdune
module Key = Key
include Dune_memory_intf
open Result.O

type 'a result = ('a, string) Result.t

let default_root () =
  Path.L.relative (Path.of_string Xdg.cache_dir) [ "dune"; "db"; "v2" ]

let promotion_to_string = function
  | Already_promoted { in_the_memory; in_the_build_directory; _ } ->
    Printf.sprintf "%s already promoted as %s"
      (Path.Local.to_string (Path.Build.local in_the_build_directory))
      (Path.to_string in_the_memory)
  | Promoted { in_the_memory; in_the_build_directory; _ } ->
    Printf.sprintf "%s promoted as %s"
      (Path.Local.to_string (Path.Build.local in_the_build_directory))
      (Path.to_string in_the_memory)

let file_of_promotion = function
  | Already_promoted f
  | Promoted f ->
    f

let command_to_dyn = function
  | Dedup { in_the_build_directory; in_the_memory; digest } ->
    let open Dyn.Encoder in
    record
      [ ("in_the_build_directory", Path.Build.to_dyn in_the_build_directory)
      ; ("in_the_memory", Path.to_dyn in_the_memory)
      ; ("digest", Digest.to_dyn digest)
      ]

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
        if Io.compare_files path file == Ordering.Eq then
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
    match
      Digest.from_hex (Path.basename (fst (Path.split_extension path)))
    with
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

let apply ~f o v =
  match o with
  | Some o -> f v o
  | None -> v

module MetadataFile = struct
  type t =
    { metadata : Sexp.t list
    ; files : File.t list
    }

  let to_sexp { metadata; files } =
    let open Sexp in
    let f ({ in_the_build_directory; in_the_memory; _ } : File.t) =
      Sexp.List
        [ Sexp.Atom
            (Path.Local.to_string (Path.Build.local in_the_build_directory))
        ; Sexp.Atom (Path.to_string in_the_memory)
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
          | List [ Atom in_the_build_directory; Atom in_the_memory ] ->
            let in_the_build_directory =
              Path.Build.of_string in_the_build_directory
            and in_the_memory = Path.of_string in_the_memory in
            Ok
              { File.in_the_memory
              ; in_the_build_directory
              ; digest = FSSchemeImpl.digest in_the_memory
              }
          | _ -> Error "invalid metadata scheme in produced files list")
      in
      { metadata; files }
    | _ -> Error "invalid metadata"

  let parse path =
    Io.with_file_in path ~f:(fun input ->
        Csexp.parse (Stream.of_channel input))
    >>= of_sexp
end

module Memory = struct
  type t =
    { root : Path.t
    ; build_root : Path.t option
    ; repositories : repository list
    ; handler : handler
    }

  let path_files memory = Path.relative memory.root "files"

  let path_meta memory = Path.relative memory.root "meta"

  let path_tmp memory = Path.relative memory.root "temp"

  let with_lock memory f =
    let lock = Lock_file.create (Path.relative memory.root ".lock") in
    let finally () = Lock_file.unlock lock in
    Exn.protect ~f ~finally

  let make_path memory path =
    match memory.build_root with
    | Some p -> Result.ok (Path.append_local p path)
    | None ->
      Result.Error
        (Format.asprintf "relative path \"%a\" while no build root was set"
           Path.Local.pp path)

  let search memory hash file =
    Collision.search (FSSchemeImpl.path (path_files memory) hash) file

  let with_repositories memory repositories = { memory with repositories }

  let promote_sync memory paths key metadata repo =
    let open Result.O in
    let* repo =
      match repo with
      | Some idx -> (
        match List.nth memory.repositories idx with
        | None ->
          Result.Error (Printf.sprintf "repository out of range: %i" idx)
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
      make_path memory (Path.Build.local path)
      >>= fun abs_path ->
      Log.infof "promote %s" (Path.to_string abs_path);
      let stat = Unix.lstat (Path.to_string abs_path) in
      ( if stat.st_kind != S_REG then
        Result.Error "invalid file type"
      else
        Result.Ok stat )
      >>= fun stat ->
      let hardlink path =
        let tmp = path_tmp memory in
        (* dune-memory uses a single writer model, the promoted file name can
           be constant *)
        let dest = Path.relative tmp "promoting" in
        if Path.exists dest then
          Path.unlink dest
        else
          Path.mkdir_p tmp;
        Path.link path dest;
        dest
      in
      let tmp = hardlink abs_path in
      let effective_hash = Digest.file_with_stats tmp (Path.stat tmp) in
      if Digest.compare effective_hash expected_hash != Ordering.Eq then (
        let message =
          Printf.sprintf "hash mismatch: %s != %s"
            (Digest.to_string effective_hash)
            (Digest.to_string expected_hash)
        in
        Log.infof "%s" message;
        Result.Error message
      ) else
        match search memory effective_hash tmp with
        | Collision.Found in_the_memory ->
          Path.unlink tmp;
          Path.touch in_the_memory;
          Result.Ok
            (Already_promoted
               { in_the_build_directory = path
               ; in_the_memory
               ; digest = effective_hash
               })
        | Collision.Not_found in_the_memory ->
          Path.mkdir_p (Path.parent_exn in_the_memory);
          let dest = Path.to_string in_the_memory in
          Unix.rename (Path.to_string tmp) dest;
          (* Remove write permissions *)
          Unix.chmod dest (stat.st_perm land 0o555);
          Result.Ok
            (Promoted
               { in_the_build_directory = path
               ; in_the_memory
               ; digest = effective_hash
               })
    in
    let f () =
      Result.List.map ~f:promote paths
      >>| fun promoted ->
      let metadata_path = FSSchemeImpl.path (path_meta memory) key
      and files = List.map ~f:file_of_promotion promoted in
      let metadata_file : MetadataFile.t = { metadata; files } in
      Path.mkdir_p (Path.parent_exn metadata_path);
      Io.write_file metadata_path
        (Csexp.to_string (MetadataFile.to_sexp metadata_file));
      let f = function
        | Already_promoted file -> memory.handler (Dedup file)
        | _ -> ()
      in
      List.iter ~f promoted;
      promoted
    in
    with_lock memory f

  let promote memory paths key metadata ~repository =
    Result.map ~f:ignore (promote_sync memory paths key metadata repository)

  let search memory key =
    let path = FSSchemeImpl.path (path_meta memory) key in
    let f () =
      let* sexp =
        try
          Io.with_file_in path ~f:(fun input ->
              Csexp.parse (Stream.of_channel input))
        with Sys_error _ -> Error "no cached file"
      in
      let+ metadata = MetadataFile.of_sexp sexp in
      (* Touch cache files so they are removed last by LRU trimming *)
      let () =
        let f (file : File.t) =
          (* There is no point in trying to trim out files that are missing :
             dune will have to check when hardlinking anyway since they could
             disappear inbetween. *)
          try Path.touch file.in_the_memory
          with Unix.(Unix_error (ENOENT, _, _)) -> ()
        in
        List.iter ~f metadata.files
      in
      (metadata.metadata, metadata.files)
    in
    with_lock memory f

  let set_build_dir memory p = { memory with build_root = Some p }

  let teardown _ = ()

  let make ?(root = default_root ()) handler =
    if Path.basename root <> "v2" then
      Result.Error "unable to read dune-memory"
    else
      Result.ok { root; build_root = None; repositories = []; handler }
end

let trimmable ?stats p =
  let stats =
    match stats with
    | None -> Path.stat p
    | Some stats -> stats
  in
  stats.Unix.st_nlink = 1

let default_trim : trimming_result =
  { trimmed_files = (0, []); trimmed_metafiles = [] }

let _garbage_collect default_trim memory =
  let path = Memory.path_meta memory in
  let metas =
    List.map ~f:(fun p -> (p, MetadataFile.parse p)) (FSSchemeImpl.list path)
  in
  let f default_trim = function
    | p, Result.Error msg ->
      Memory.with_lock memory (fun () ->
          Log.infof "remove invalid metadata file %a: %s" Path.pp p msg;
          Path.unlink_no_err p;
          { default_trim with trimmed_metafiles = [ p ] })
    | p, Result.Ok { MetadataFile.files; _ } ->
      if
        not
          (List.for_all
             ~f:(fun { File.in_the_memory; _ } -> Path.exists in_the_memory)
             files)
      then
        Memory.with_lock memory (fun () ->
            Log.infof
              "remove metadata file %a as some produced files are missing"
              Path.pp p;
            let res =
              List.fold_left ~init:default_trim
                ~f:(fun ({ trimmed_files = size, files; _ } as trim) f ->
                  let p = f.File.in_the_memory in
                  try
                    let stats = Path.stat p in
                    if trimmable p then (
                      Path.unlink_no_err p;
                      { trim with
                        trimmed_files = (size + stats.st_size, p :: files)
                      }
                    ) else
                      trim
                  with Unix.Unix_error (Unix.ENOENT, _, _) -> trim)
                files
            in
            Path.unlink_no_err p;
            res)
      else
        default_trim
  in
  List.fold_left ~init:default_trim ~f metas

let garbage_collect = _garbage_collect default_trim

let trim memory free =
  let path = Memory.path_files memory in
  let files = FSSchemeImpl.list path in
  let f path =
    let stats = Path.stat path in
    if trimmable ~stats path then
      Some (path, stats.st_size, stats.st_mtime)
    else
      None
  and compare (_, _, t1) (_, _, t2) =
    Ordering.of_int (Pervasives.compare t1 t2)
  in
  let files = List.sort ~compare (List.filter_map ~f files)
  and delete ({ trimmed_files = freed, files; _ } as trim) (path, size, _) =
    if freed >= free then
      trim
    else (
      Path.unlink path;
      { trim with trimmed_files = (freed + size, path :: files) }
    )
  in
  let trim =
    Memory.with_lock memory (fun () ->
        List.fold_left ~init:default_trim ~f:delete files)
  in
  _garbage_collect trim memory

let size memory =
  let root = Memory.path_files memory in
  let files = FSSchemeImpl.list root in
  let stats =
    let f p =
      try (Path.stat p).st_size with Unix.Unix_error (Unix.ENOENT, _, _) -> 0
    in
    List.map ~f files
  in
  List.fold_left ~f:(fun acc size -> acc + size) ~init:0 stats

let make_caching (type t) (module Caching : Memory with type t = t) (cache : t)
    : (module Caching) =
  ( module struct
    module Cache = Caching

    let cache = cache
  end )
