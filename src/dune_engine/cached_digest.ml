open Import

(* The reduced set of file stats this module inspects to decide whether a file
   changed or not *)
module Reduced_stats = struct
  type t =
    { mtime : float
    ; size : int
    ; perm : Unix.file_perm
    }

  let to_dyn { mtime; size; perm } =
    Dyn.Record
      [ ("mtime", Float mtime); ("size", Int size); ("perm", Int perm) ]

  let of_unix_stats (stats : Unix.stats) =
    { mtime = stats.st_mtime; size = stats.st_size; perm = stats.st_perm }

  let compare a b =
    match Float.compare a.mtime b.mtime with
    | (Lt | Gt) as x -> x
    | Eq -> (
      match Int.compare a.size b.size with
      | (Lt | Gt) as x -> x
      | Eq -> Int.compare a.perm b.perm)
end

type file =
  { mutable digest : Digest.t
  ; mutable stats : Reduced_stats.t
  ; mutable stats_checked : int
  }

type t =
  { mutable checked_key : int
  ; mutable max_timestamp : float
  ; table : file Path.Table.t
  }

let db_file = Path.relative Path.build_dir ".digest-db"

let dyn_of_file { digest; stats; stats_checked } =
  Dyn.Record
    [ ("digest", Digest.to_dyn digest)
    ; ("stats", Reduced_stats.to_dyn stats)
    ; ("stats_checked", Int stats_checked)
    ]

let to_dyn { checked_key; max_timestamp; table } =
  Dyn.Record
    [ ("checked_key", Int checked_key)
    ; ("max_timestamp", Float max_timestamp)
    ; ("table", Path.Table.to_dyn dyn_of_file table)
    ]

module P = Persistent.Make (struct
  type nonrec t = t

  let name = "DIGEST-DB"

  let version = 5

  let to_dyn = to_dyn
end)

let needs_dumping = ref false

(* CR-someday amokhov: replace this mutable table with a memoized function. This
   will probably require splitting this module in two, for dealing with source
   and target files, respectively. For source files, we receive updates via the
   file-watching API. For target files, we modify the digests ourselves, without
   subscribing for file-watching updates. *)
let cache =
  lazy
    (match P.load db_file with
    | None ->
      { checked_key = 0; table = Path.Table.create 1024; max_timestamp = 0. }
    | Some cache ->
      cache.checked_key <- cache.checked_key + 1;
      cache)

let get_current_filesystem_time () =
  let special_path = Path.relative Path.build_dir ".filesystem-clock" in
  Io.write_file special_path "<dummy>";
  (Path.stat special_path).st_mtime

let wait_for_fs_clock_to_advance () =
  let t = get_current_filesystem_time () in
  while get_current_filesystem_time () <= t do
    (* This is a blocking wait but we don't care too much. This code is only
       used in the test suite. *)
    Unix.sleepf 0.01
  done

let delete_very_recent_entries () =
  let cache = Lazy.force cache in
  if !Clflags.wait_for_filesystem_clock then wait_for_fs_clock_to_advance ();
  let now = get_current_filesystem_time () in
  match Float.compare cache.max_timestamp now with
  | Lt -> ()
  | Eq
  | Gt ->
    Path.Table.filteri_inplace cache.table ~f:(fun ~key:fn ~data ->
        match Float.compare data.stats.mtime now with
        | Lt -> true
        | Gt
        | Eq ->
          if !Clflags.debug_digests then
            Console.print
              [ Pp.textf
                  "Dropping cached digest for %s because it has exactly the \
                   same mtime as the file system clock."
                  (Path.to_string_maybe_quoted fn)
              ];
          false)

let dump () =
  if !needs_dumping && Path.build_dir_exists () then (
    needs_dumping := false;
    delete_very_recent_entries ();
    P.dump db_file (Lazy.force cache)
  )

let () = Hooks.End_of_build.always dump

let invalidate_cached_timestamps () =
  (if Lazy.is_val cache then
    let cache = Lazy.force cache in
    cache.checked_key <- cache.checked_key + 1);
  delete_very_recent_entries ()

let set_max_timestamp cache (stat : Unix.stats) =
  cache.max_timestamp <- Float.max cache.max_timestamp stat.st_mtime

let set_with_stat fn digest stat =
  let cache = Lazy.force cache in
  needs_dumping := true;
  set_max_timestamp cache stat;
  Path.Table.set cache.table fn
    { digest
    ; stats = Reduced_stats.of_unix_stats stat
    ; stats_checked = cache.checked_key
    }

let set fn digest =
  let stat = Path.stat fn in
  set_with_stat fn digest stat

let refresh_ stats fn =
  let digest = Digest.file_with_stats fn stats in
  set_with_stat fn digest stats;
  digest

let refresh_internal fn =
  let stats = Path.stat fn in
  refresh_ stats fn

let refresh fn = refresh_internal (Path.build fn)

let refresh_and_chmod fn =
  let fn = Path.build fn in
  let stats = Path.lstat fn in
  let stats =
    match stats.st_kind with
    | S_LNK ->
      (* If the path is a symbolic link, we don't try to remove write
         permissions. For two reasons:

         - if the destination was not a build path (i.e. in the build
         directory), then it would definitely be wrong to do so

         - if it is in the build directory, then we expect that the rule
         producing this file will have taken core of chmodding it *)
      Path.stat fn
    | _ -> (
      match Cache.cachable stats.st_kind with
      | true ->
        (* We remove write permissions to uniformize behavior regardless of
           whether the cache is activated. No need to be zealous in case the
           file is not cached anyway. See issue #3311. *)
        let perm = stats.st_perm land lnot 0o222 in
        Path.chmod ~mode:perm fn;
        { stats with st_perm = perm }
      | false -> stats)
  in
  refresh_ stats fn

let peek_file fn =
  let cache = Lazy.force cache in
  match Path.Table.find cache.table fn with
  | None -> None
  | Some x ->
    Some
      (if x.stats_checked = cache.checked_key then
        x.digest
      else
        let stats = Path.stat fn in
        let reduced_stats = Reduced_stats.of_unix_stats stats in
        match Reduced_stats.compare x.stats reduced_stats with
        | Eq ->
          (* Even though we're modifying the [stats_checked] field, we don't
             need to set [needs_dumping := true] here. This is because
             [checked_key] is incremented every time we load from disk, which
             makes it so that [stats_checked < checked_key] for all entries
             after loading, regardless of whether we save the new value here or
             not. *)
          x.stats_checked <- cache.checked_key;
          x.digest
        | Gt
        | Lt ->
          let digest = Digest.file_with_stats fn stats in
          if !Clflags.debug_digests then
            Console.print
              [ Pp.textf "Re-digested file %s because its stats changed:"
                  (Path.to_string_maybe_quoted fn)
              ; Dyn.pp
                  (Dyn.Record
                     [ ("old_digest", Digest.to_dyn x.digest)
                     ; ("new_digest", Digest.to_dyn digest)
                     ; ("old_stats", Reduced_stats.to_dyn x.stats)
                     ; ("new_stats", Reduced_stats.to_dyn reduced_stats)
                     ])
              ];
          needs_dumping := true;
          set_max_timestamp cache stats;
          x.digest <- digest;
          x.stats <- reduced_stats;
          x.stats_checked <- cache.checked_key;
          digest)

let peek_or_refresh_file fn =
  match peek_file fn with
  | None -> refresh_internal fn
  | Some v -> v

let source_or_external_file fn =
  let open Memo.Build.O in
  assert (not (Path.is_in_build_dir fn));
  let res = peek_or_refresh_file fn in
  let+ () = Fs_notify_memo.depend fn in
  res

let build_file fn = peek_or_refresh_file (Path.build fn)

let remove fn =
  let cache = Lazy.force cache in
  needs_dumping := true;
  Path.Table.remove cache.table fn
