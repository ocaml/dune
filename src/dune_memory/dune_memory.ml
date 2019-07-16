open Stdune
open Utils

type memory = {root: Path.t; log: Log.t}

type key = Digest.t

type metadata = Sexp.t list

type promotion =
  | Already_promoted of Path.t * Path.t
  | Promoted of Path.t * Path.t
  | Hash_mismatch of Path.t * Digest.t * Digest.t

let compare a b : ordering =
  let a = Path.to_string a and b = Path.to_string b in
  let lena = String.length a and lenb = String.length b in
  let len = min lena lenb in
  let rec loop i =
    if i = len then Int.compare lena lenb
    else
      match Char.compare a.[i] b.[i] with
      | 0 ->
          loop (i + 1)
      | x when x < 0 ->
          Ordering.Lt
      | _ ->
          Ordering.Gt
  in
  loop 0

let with_lock memory f =
  let lock =
    Stdune.Lockf.lock (Path.to_string (Path.L.relative memory.root [".lock"]))
  in
  let finally () = Stdune.Lockf.unlock lock in
  Exn.protect ~f ~finally

let key consumed metadata produced =
  let consumed =
    List.sort ~compare:(fun (p1, _) (p2, _) -> compare p1 p2) consumed
  and produced = List.sort ~compare produced in
  let key =
    Sexp.List
      [ Sexp.List
          (List.map
             ~f:(fun (p, h) ->
               Sexp.List
                 [Sexp.Atom (Path.to_string p); Sexp.Atom (Digest.to_string h)]
               )
             consumed)
      ; Sexp.List metadata
      ; Sexp.List
          (List.map ~f:(fun p -> Sexp.Atom (Path.to_string p)) produced) ]
  in
  Digest.string (Csexp.to_string key)

let key_to_string = Digest.to_string

let key_of_string = Digest.from_hex

let promotion_to_string = function
  | Already_promoted (original, promoted) ->
      Printf.sprintf "%s already promoted as %s" (Path.to_string original)
        (Path.to_string promoted)
  | Promoted (original, promoted) ->
      Printf.sprintf "%s promoted as %s" (Path.to_string original)
        (Path.to_string promoted)
  | Hash_mismatch (original, expected, effective) ->
      Printf.sprintf "hash for %s mismatch: expected %s got %s"
        (Path.to_string original)
        (Digest.to_string expected)
        (Digest.to_string effective)

exception Failed = Utils.Failed

let path_files memory = Path.L.relative memory.root ["files"]

let path_meta memory = Path.L.relative memory.root ["meta"]

let path_tmp memory = Path.L.relative memory.root ["temp"]

let default_root () =
  Path.L.relative (Path.of_string Xdg.cache_dir) ["dune"; "db"; "v2"]

let make ?log ?(root = default_root ()) () =
  if Path.basename root <> "v2" then
    Result.Error (Failure "unable to read dune-memory")
  else
    Result.ok
      {root; log= (match log with Some log -> log | None -> Log.no_log)}

(* How to handle collisions. E.g. another version could assume collisions are not possible *)
module Collision = struct
  type res = Found of Path.t | Not_found of Path.t

  (* We need to ensure we do not create holes in the suffix numbering for this to work *)
  let search path file =
    let rec loop n =
      let path = Path.extend_basename path ~suffix:("." ^ string_of_int n) in
      if Sys.file_exists (Path.to_string path) then
        if Io.compare_files path file == Ordering.Eq then Found path
        else loop (n + 1)
      else Not_found path
    in
    loop 1
end

module type FSScheme = sig
  val path : Path.t -> Digest.t -> Path.t

  val list : Path.t -> Path.t list
end

(* Where to store file with a given hash. In this case ab/abcdef. *)
module FirstTwoCharsSubdir : FSScheme = struct
  let path root hash =
    let hash = Digest.to_string hash in
    let short_hash = String.sub hash ~pos:0 ~len:2 in
    Path.L.relative root [short_hash; hash]

  let list root =
    let f dir =
      let is_hex_char c =
        let char_in s e = Char.compare c s >= 0 && Char.compare c e <= 0 in
        char_in 'a' 'f' || char_in '0' '9'
      and root = Path.L.relative root [dir] in
      if String.for_all ~f:is_hex_char dir then
        Array.map
          ~f:(fun filename -> Path.L.relative root [filename])
          (Sys.readdir (Path.to_string root))
      else Array.of_list []
    in
    Array.to_list
      (Array.concat
         (Array.to_list (Array.map ~f (Sys.readdir (Path.to_string root)))))
end

module FSSchemeImpl = FirstTwoCharsSubdir

let search memory hash file =
  Collision.search (FSSchemeImpl.path (path_files memory) hash) file

let apply ~f o v = match o with Some o -> f v o | None -> v

let promote memory paths key metadata repo =
  let metadata =
    apply
      ~f:(fun metadata (remote, commit) ->
        metadata
        @ [ Sexp.List [Sexp.Atom "repo"; Sexp.Atom remote]
          ; Sexp.List [Sexp.Atom "commit_id"; Sexp.Atom commit] ] )
      repo metadata
  in
  let promote (path, expected_hash) =
    Log.infof memory.log "promote %s" (Path.to_string path) ;
    let hardlink path =
      let tmp = path_tmp memory in
      (* dune-memory uses a single writer model, the promoted file name can be constant *)
      let dest = Path.L.relative tmp ["promoting"] in
      (let dest = Path.to_string dest in
       if Sys.file_exists dest then Unix.unlink dest else mkpath tmp ;
       Unix.link (Path.to_string path) dest) ;
      dest
    in
    let tmp = hardlink path in
    let effective_hash = Digest.file tmp in
    if Digest.compare effective_hash expected_hash != Ordering.Eq then (
      Log.infof memory.log "hash mismatch: %s != %s"
        (Digest.to_string effective_hash)
        (Digest.to_string expected_hash) ;
      Hash_mismatch (path, expected_hash, effective_hash) )
    else
      match search memory effective_hash tmp with
      | Collision.Found p ->
          Unix.unlink (Path.to_string tmp) ;
          Already_promoted (path, p)
      | Collision.Not_found p ->
          mkpath (Path.parent_exn p) ;
          let dest = Path.to_string p in
          Unix.rename (Path.to_string tmp) dest ;
          (* Remove write permissions *)
          Unix.chmod dest ((Unix.stat dest).st_perm land 0o555) ;
          Promoted (path, p)
  in
  let f () =
    unix (fun () ->
        let res = List.map ~f:promote paths
        and metadata_path = FSSchemeImpl.path (path_meta memory) key in
        mkpath (Path.parent_exn metadata_path) ;
        Io.write_file metadata_path
          (Csexp.to_string
             (Sexp.List
                [ Sexp.List (Sexp.Atom "metadata" :: metadata)
                ; Sexp.List
                    [ Sexp.Atom "produced-files"
                    ; Sexp.List
                        (List.filter_map
                           ~f:(function
                             | Promoted (o, p) | Already_promoted (o, p) ->
                                 Some
                                   (Sexp.List
                                      [ Sexp.Atom (Path.to_string o)
                                      ; Sexp.Atom (Path.to_string p) ])
                             | _ ->
                                 None )
                           res) ] ])) ;
        res )
  in
  with_lock memory f

let search memory key =
  let path = FSSchemeImpl.path (path_meta memory) key in
  let metadata =
    Result.ok_exn
      (Result.map_error
         ~f:(fun r -> Failure r)
         (let open Result.O in
         Io.with_file_in path ~f:(fun input ->
             Csexp.parse (Stream.of_channel input) )
         >>= function
         | Sexp.List l -> Result.ok l | _ -> Result.Error "invalid metadata"))
  in
  let f () =
    match metadata with
    | [ Sexp.List (Sexp.Atom s_metadata :: metadata)
      ; Sexp.List [Sexp.Atom s_produced; Sexp.List produced] ] ->
        if
          (not (String.equal s_metadata "metadata"))
          && String.equal s_produced "produced-files"
        then raise (Failed "invalid metadata scheme: wrong key")
        else
          ( metadata
          , List.map produced ~f:(function
              | Sexp.List [Sexp.Atom f; Sexp.Atom t] ->
                  (Path.of_string f, Path.of_string t)
              | _ ->
                  raise
                    (Failed "invalid metadata scheme in produced files list") )
          )
    | _ ->
        raise (Failed "invalid metadata scheme")
  in
  with_lock memory f

let trim memory free =
  let path = path_files memory in
  let files = FSSchemeImpl.list path in
  let f path =
    let stat = Unix.stat (Path.to_string path) in
    if stat.st_nlink = 1 then Some (path, stat.st_size, stat.st_ctime)
    else None
  and compare (_, _, t1) (_, _, t2) =
    Ordering.of_int (Pervasives.compare t1 t2)
  in
  let files = List.sort ~compare (List.filter_map ~f files)
  and delete (freed, res) (path, size, _) =
    if freed >= free then (freed, res)
    else (
      Unix.unlink (Path.to_string path) ;
      (freed + size, path :: res) )
  in
  with_lock memory (fun () -> List.fold_left ~init:(0, []) ~f:delete files)
