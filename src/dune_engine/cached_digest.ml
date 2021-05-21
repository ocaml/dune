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
  (Path.Untracked.stat_exn special_path).st_mtime

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
    Console.Status_line.set_live_temporarily
      (fun () -> Some (Pp.hbox (Pp.text "Saving digest db...")))
      (fun () ->
        delete_very_recent_entries ();
        P.dump db_file (Lazy.force cache))
  )

let () = at_exit dump

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
  (* the caller of [set] ensures that the files exist *)
  let stat = Path.Untracked.stat_exn fn in
  set_with_stat fn digest stat

let refresh_exn stats fn =
  let digest = Digest.file_with_stats fn stats in
  set_with_stat fn digest stats;
  digest

let refresh_internal_exn fn =
  let stats = Path.Untracked.stat_exn fn in
  refresh_exn stats fn

module Refresh_result = struct
  type t =
    | Ok of Digest.t
    | No_such_file
    | Error of exn
end

let catch_fs_errors f =
  match f () with
  | exception ((Unix.Unix_error _ | Sys_error _) as exn) ->
    Refresh_result.Error exn
  | res -> res

let refresh fn ~remove_write_permissions : Refresh_result.t =
  let fn = Path.build fn in
  catch_fs_errors (fun () ->
      match Path.Untracked.lstat_exn fn with
      | exception Unix.Unix_error (ENOENT, _, _) -> Refresh_result.No_such_file
      | stats ->
        let stats =
          match stats.st_kind with
          | Unix.S_LNK -> (
            try Path.Untracked.stat_exn fn with
            | Unix.Unix_error (ENOENT, _, _) ->
              raise (Sys_error "Broken symlink"))
          | Unix.S_REG -> (
            match remove_write_permissions with
            | false -> stats
            | true ->
              let perm =
                Path.Permissions.remove ~mode:Path.Permissions.write
                  stats.st_perm
              in
              Path.chmod ~mode:perm fn;
              { stats with st_perm = perm })
          | _ -> stats
        in
        Refresh_result.Ok (refresh_exn stats fn))

let peek_file fn =
  let cache = Lazy.force cache in
  match Path.Table.find cache.table fn with
  | None -> None
  | Some x ->
    Some
      (if x.stats_checked = cache.checked_key then
        x.digest
      else
        let stats = Path.Untracked.stat_exn fn in
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
  | None -> refresh_internal_exn fn
  | Some v -> v

let source_or_external_file = peek_or_refresh_file

let build_file fn = peek_or_refresh_file (Path.build fn)

let remove fn =
  let cache = Lazy.force cache in
  needs_dumping := true;
  Path.Table.remove cache.table fn
