open Stdune
open Utils

type key = Digest.t

type metadata = Dune_lang.t list

type 'a result = ('a, string) Result.t

let default_root () =
  Path.L.relative (Path.of_string Xdg.cache_dir) [ "dune"; "db"; "v2" ]

type promotion =
  | Already_promoted of Path.t * Path.t
  | Promoted of Path.t * Path.t

let key_to_string = Digest.to_string

let key_of_string s =
  match Digest.from_hex s with
  | Some d -> Result.Ok d
  | None -> Result.Error (Printf.sprintf "invalid key: %s" s)

let promotion_to_string = function
  | Already_promoted (original, promoted) ->
    Printf.sprintf "%s already promoted as %s" (Path.to_string original)
      (Path.to_string promoted)
  | Promoted (original, promoted) ->
    Printf.sprintf "%s promoted as %s" (Path.to_string original)
      (Path.to_string promoted)

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
      if Sys.file_exists (Path.to_string path) then
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
          ~f:(fun filename -> Path.L.relative root [ filename ])
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

module File = struct
  type t =
    { in_the_memory : Path.t
    ; in_the_build_directory : Path.t
    ; digest : Digest.t
    }
end

module Search_result = struct
  type t =
    | Found of metadata * File.t list
    | Not_found
    | Cannot_read of exn
end

module type memory = sig
  type t

  val promote :
       t
    -> (Path.t * Digest.t) list
    -> key
    -> metadata
    -> (string * string) option
    -> (promotion list, string) Result.t

  val search : t -> ?touch:bool -> key -> Search_result.t
end

module Memory = struct
  type t = { root : Path.t }

  let path_files memory = Path.L.relative memory.root [ "files" ]

  let path_meta memory = Path.L.relative memory.root [ "meta" ]

  let path_tmp memory = Path.L.relative memory.root [ "temp" ]

  let with_lock memory f =
    let lock =
      Stdune.Lock_file.create (Path.L.relative memory.root [ ".lock" ])
    in
    let finally () = Stdune.Lock_file.unlock lock in
    Exn.protect ~f ~finally

  let search memory hash file =
    Collision.search (FSSchemeImpl.path (path_files memory) hash) file

  module M = Dune_lang.Versioned_file.Make (Unit)

  let syntax =
    Dune_lang.Syntax.create ~name:"dune-memory" ~desc:"Dune memory metadata"
      [ (1, 0) ]

  let () = M.Lang.register syntax ()

  let promote memory paths key metadata repo =
    let open Result.O in
    let metadata =
      apply
        ~f:(fun metadata (remote, commit) ->
          metadata
          @ [ Dune_lang.List [ Dune_lang.atom "repo"; Dune_lang.atom remote ]
            ; List [ Dune_lang.atom "commit_id"; Dune_lang.atom commit ]
            ])
        repo metadata
    in
    let promote (path, expected_hash) =
      Log.infof "promote %s" (Path.to_string path);
      let stat = Unix.lstat (Path.to_string path) in
      ( if stat.st_kind != S_REG then
        Result.Error "invalid file type"
      else
        Result.Ok stat )
      >>= fun stat ->
      let hardlink path =
        let tmp = path_tmp memory in
        (* dune-memory uses a single writer model, the promoted file name can
           be constant *)
        let dest = Path.L.relative tmp [ "promoting" ] in
        (let dest = Path.to_string dest in
         if Sys.file_exists dest then
           Unix.unlink dest
         else
           mkpath tmp;
         Unix.link (Path.to_string path) dest);
        dest
      in
      let tmp = hardlink path in
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
        | Collision.Found p ->
          Unix.unlink (Path.to_string tmp);
          Path.touch p;
          Result.Ok (Already_promoted (path, p))
        | Collision.Not_found p ->
          mkpath (Path.parent_exn p);
          let dest = Path.to_string p in
          Unix.rename (Path.to_string tmp) dest;
          (* Remove write permissions *)
          Unix.chmod dest (stat.st_perm land 0o555);
          Result.Ok (Promoted (path, p))
    in
    let f () =
      Result.List.map ~f:promote paths
      >>| fun promoted ->
      let metadata_path = FSSchemeImpl.path (path_meta memory) key in
      mkpath (Path.parent_exn metadata_path);
      Io.with_file_out metadata_path ~f:(fun oc ->
          Dune_lang.output oc
            (List
               [ Dune_lang.atom "lang"
               ; Dune_lang.atom "dune-memory"
               ; Dune_lang.atom "1.0"
               ]);
          output_char oc '\n';
          Dune_lang.output oc (List (Dune_lang.atom "metadata" :: metadata));
          Dune_lang.output oc
            (List
               ( Dune_lang.atom "files"
               :: List.map
                    ~f:(function
                      | Promoted (o, p)
                       |Already_promoted (o, p) ->
                        Dune_lang.List
                          [ Dune_lang.atom_or_quoted_string (Path.to_string o)
                          ; Dune_lang.atom_or_quoted_string (Path.to_string p)
                          ])
                    promoted )));
      promoted
    in
    with_lock memory f

  let decode =
    let open Dune_lang.Decoder in
    fields
      (let+ metadata = field "metadata" (repeat raw)
       and+ files =
         field "files"
           (repeat
              (let+ in_the_build_directory, in_the_memory =
                 pair string string
               in
               let in_the_build_directory =
                 Path.of_string in_the_build_directory
               and in_the_memory = Path.of_string in_the_memory in
               { File.in_the_memory
               ; in_the_build_directory
               ; digest = FSSchemeImpl.digest in_the_memory
               }))
       in
       (List.map metadata ~f:Dune_lang.Ast.remove_locs, files))

  let search memory ?(touch = true) key =
    let path = FSSchemeImpl.path (path_meta memory) key in
    let f () : Search_result.t =
      match Io.open_in path with
      | exception Sys_error _ -> Not_found
      | ic ->
        let res : Search_result.t =
          match
            M.parse
              (Lexbuf.from_channel ic ~fname:(Path.to_string path))
              ~f:(fun _ -> decode)
          with
          | metadata, files -> Found (metadata, files)
          | exception exn -> Cannot_read exn
        in
        Io.close_in ic;
        res
    in
    let res = with_lock memory f in
    ( if touch then
      match res with
      | Found (_, files) ->
        List.iter files ~f:(fun f -> Path.touch f.File.in_the_memory)
      | _ -> () );
    res
end

let make ?(root = default_root ()) () =
  if Path.basename root <> "v2" then
    Result.Error "unable to read dune-memory"
  else
    Result.ok { Memory.root }

let trim memory free =
  let path = Memory.path_files memory in
  let files = FSSchemeImpl.list path in
  let f path =
    let stat = Unix.stat (Path.to_string path) in
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
      Unix.unlink (Path.to_string path);
      (freed + size, path :: res)
    )
  in
  Memory.with_lock memory (fun () ->
      List.fold_left ~init:(0, []) ~f:delete files)
