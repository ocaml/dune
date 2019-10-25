open Stdune
open Utils
include Dune_memory_intf

type 'a result = ('a, string) Result.t

let default_root () =
  Path.L.relative (Path.of_string Xdg.cache_dir) [ "dune"; "db"; "v2" ]

let key_to_string = Digest.to_string

let key_of_string s =
  match Digest.from_hex s with
  | Some d -> Result.Ok d
  | None -> Result.Error (Printf.sprintf "invalid key: %s" s)

let promotion_to_string = function
  | Already_promoted { in_the_memory; in_the_build_directory; _ } ->
    Printf.sprintf "%s already promoted as %s"
      (Path.Local.to_string (Path.Build.local in_the_build_directory))
      (Path.to_string in_the_memory)
  | Promoted { in_the_memory; in_the_build_directory; _ } ->
    Printf.sprintf "%s promoted as %s"
      (Path.Local.to_string (Path.Build.local in_the_build_directory))
      (Path.to_string in_the_memory)

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
        Array.map
          ~f:(Path.relative root)
          (Sys.readdir (Path.to_string root))
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
        (if Path.exists dest then
           Path.unlink dest
         else
           mkpath tmp;
         Path.link path dest);
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
          mkpath (Path.parent_exn in_the_memory);
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
      let metadata_path = FSSchemeImpl.path (path_meta memory) key in
      mkpath (Path.parent_exn metadata_path);
      Io.write_file metadata_path
        (Csexp.to_string
           (List
              [ List (Atom "metadata" :: metadata)
              ; List
                  ( Atom "files"
                  :: List.map
                       ~f:(function
                         | Promoted
                             { in_the_build_directory; in_the_memory; _ }
                         | Already_promoted
                             { in_the_build_directory; in_the_memory; _ } ->
                           Sexp.List
                             [ Sexp.Atom
                                 (Path.Local.to_string
                                    (Path.Build.local in_the_build_directory))
                             ; Sexp.Atom (Path.to_string in_the_memory)
                             ])
                       promoted )
              ]));
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
      let open Result.O in
      ( try
          Io.with_file_in path ~f:(fun input ->
              Csexp.parse (Stream.of_channel input))
        with Sys_error _ -> Error "no cached file" )
      >>= function
      | Sexp.List
          [ List (Atom "metadata" :: metadata)
          ; List (Atom "files" :: produced)
          ] ->
        let+ produced =
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
        (* Touch cache files so they are removed last by LRU trimming *)
        List.iter produced ~f:(fun f -> Path.touch f.File.in_the_memory);
        (metadata, produced)
      | _ -> Error "invalid metadata"
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

let trim memory free =
  let path = Memory.path_files memory in
  let files = FSSchemeImpl.list path in
  let f path =
    let stat = Path.stat path in
    if stat.st_nlink = 1 then
      Some (path, stat.st_size, stat.st_ctime)
    else
      None
  and compare (_, _, t1) (_, _, t2) =
    Ordering.of_int (Pervasives.compare t1 t2)
  in
  let files = List.sort ~compare (List.filter_map ~f files)
  and delete (freed, res) (path, size, _) =
    if freed >= free then
      (freed, res)
    else (
      Path.unlink path;
      (freed + size, path :: res)
    )
  in
  Memory.with_lock memory (fun () ->
      List.fold_left ~init:(0, []) ~f:delete files)

let make_caching (type t) (module Caching : Memory with type t = t) (cache : t)
    : (module Caching) =
  ( module struct
    module Cache = Caching

    let cache = cache
  end )
