open Stdune

type file =
  { mutable digest        : Digest.t
  ; mutable timestamp     : float
  ; mutable size          : int
  ; mutable permissions   : Unix.file_perm
  ; mutable stats_checked : int
  }

type t =
  { mutable checked_key : int
  ; mutable max_timestamp : float
  ; table               : file Path.Table.t
  }

let db_file = Path.relative Path.build_dir ".digest-db"

module P = Persistent.Make(struct
    type nonrec t = t
    let name = "DIGEST-DB"
    let version = 4
  end)

let needs_dumping = ref false

let cache = lazy (
  match P.load db_file with
  | None ->
    { checked_key = 0
    ; table = Path.Table.create 1024
    ; max_timestamp = 0.
    }
  | Some cache ->
    cache.checked_key <- cache.checked_key + 1;
    cache)

let get_current_filesystem_time () =
  let special_path = Path.relative Path.build_dir ".filesystem-clock" in
  Io.write_file special_path "<dummy>";
  (Path.stat special_path).st_mtime

let delete_very_recent_entries () =
  let cache = Lazy.force cache in
  let now = get_current_filesystem_time () in
  match Float.compare cache.max_timestamp now with
  | Lt -> ()
  | Eq | Gt ->
    Path.Table.filteri_inplace cache.table ~f:(fun ~key:_ ~data ->
      match Float.compare data.timestamp now with
      | Lt -> true
      | Gt | Eq -> false
    )

let dump () =
  if !needs_dumping && Path.build_dir_exists () then begin
    needs_dumping := false;
    delete_very_recent_entries ();
    P.dump db_file (Lazy.force cache)
  end

let () = Hooks.End_of_build.always dump

let invalidate_cached_timestamps () =
  if Lazy.is_val cache then begin
    let cache = Lazy.force cache in
    cache.checked_key <- cache.checked_key + 1
  end;
  delete_very_recent_entries ()

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

let set_max_timestamp cache (stat : Unix.stats) =
  cache.max_timestamp <- Float.max cache.max_timestamp stat.st_mtime

let refresh fn =
  let cache = Lazy.force cache in
  let stat = Path.stat fn in
  let permissions = stat.st_perm in
  let digest = path_stat_digest fn stat in
  needs_dumping := true;
  set_max_timestamp cache stat;
  Path.Table.replace cache.table ~key:fn
    ~data:{ digest
          ; timestamp = stat.st_mtime
          ; stats_checked = cache.checked_key
          ; size = stat.st_size
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
      set_max_timestamp cache stat;
      if stat.st_mtime <> x.timestamp then begin
        dirty := true;
        x.timestamp <- stat.st_mtime
      end;
      if stat.st_perm <> x.permissions then begin
        dirty := true;
        x.permissions <- stat.st_perm
      end;
      if stat.st_size <> x.size then begin
        dirty := true;
        x.size <- stat.st_size
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
