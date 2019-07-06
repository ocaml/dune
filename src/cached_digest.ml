open Stdune

type file =
  { mutable digest        : Digest.t
  ; mutable timestamp     : float
  ; mutable permissions   : Unix.file_perm
  ; mutable stats_checked : int
  }

type t =
  { mutable checked_key : int
  ; table               : file Path.Table.t
  }

let db_file = Path.relative Path.build_dir ".digest-db"

module P = Persistent.Make(struct
    type nonrec t = t
    let name = "DIGEST-DB"
    let version = 2
  end)

let needs_dumping = ref false

let cache = lazy (
  match P.load db_file with
  | None ->
    { checked_key = 0
    ; table = Path.Table.create 1024
    }
  | Some cache ->
    cache.checked_key <- cache.checked_key + 1;
    cache)

let dump () =
  if !needs_dumping && Path.build_dir_exists () then begin
    needs_dumping := false;
    P.dump db_file (Lazy.force cache)
  end

let () = Hooks.End_of_build.always dump

let invalidate_cached_timestamps () =
  if Lazy.is_val cache then begin
    let cache = Lazy.force cache in
    cache.checked_key <- cache.checked_key + 1
  end

let dir_digest (stat : Unix.stats) =
  Digest.generic
    ( stat.st_size
    , stat.st_perm
    , stat.st_mtime
    , stat.st_ctime
    )

let path_stat_digest fn stat =
  if stat.Unix.st_kind = Unix.S_DIR then
    dir_digest stat
  else
    Digest.generic (Digest.file fn, stat.st_perm)

let refresh fn =
  let cache = Lazy.force cache in
  let stat = Path.stat fn in
  let permissions = stat.st_perm in
  let digest = path_stat_digest fn stat in
  needs_dumping := true;
  Path.Table.replace cache.table ~key:fn
    ~data:{ digest
          ; timestamp = stat.st_mtime
          ; stats_checked = cache.checked_key
          ; permissions
          };
  digest

let file fn =
  let cache = Lazy.force cache in
  match Path.Table.find cache.table fn with
  | None -> refresh fn
  | Some x ->
    if x.stats_checked = cache.checked_key then
      x.digest
    else begin
      needs_dumping := true;
      let stat = Path.stat fn in
      let dirty = ref false in
      if stat.st_mtime <> x.timestamp then begin
        dirty := true;
        x.timestamp <- stat.st_mtime
      end;
      if stat.st_perm <> x.permissions then begin
        dirty := true;
        x.permissions <- stat.st_perm
      end;
      if !dirty then
        x.digest <- path_stat_digest fn stat;
      x.stats_checked <- cache.checked_key;
      x.digest
    end

let remove fn =
  let cache = Lazy.force cache in
  needs_dumping := true;
  Path.Table.remove cache.table fn
