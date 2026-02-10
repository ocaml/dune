open Import

module Annot = struct
  type t =
    { in_source : Path.Source.t
    ; in_build : Path.Build.t
    }

  let to_dyn { in_source; in_build } =
    let open Dyn in
    record
      [ "in_source", Path.Source.to_dyn in_source
      ; "in_build", Path.Build.to_dyn in_build
      ]
  ;;

  let annot = User_message.Annots.Key.create ~name:"promote" to_dyn
end

let staging_area = Path.Build.relative Path.Build.root ".promotion-staging"

module File = struct
  type t =
    { src : Path.Build.t
    ; staging : Path.Build.t option
    ; dst : Path.Source.t
    }

  (* CR-soon rgrinberg: rename either this accessor or the function *)
  let source t = t.dst
  let correction_file { src; staging; _ } = Path.build (Option.value staging ~default:src)

  let compare { src; staging; dst } t =
    let open Ordering.O in
    let= () = Path.Build.compare src t.src in
    let= () = Option.compare Path.Build.compare staging t.staging in
    let= () = Path.Source.compare dst t.dst in
    Eq
  ;;

  let in_staging_area source = Path.Build.append_source staging_area source

  let to_dyn { src; staging; dst } =
    let open Dyn in
    record
      [ "src", Path.Build.to_dyn src
      ; "staging", option Path.Build.to_dyn staging
      ; "dst", Path.Source.to_dyn dst
      ]
  ;;

  let do_promote ~correction_file ~dst =
    (match Fpath.unlink (Path.Source.to_string dst) with
     | Success | Does_not_exist -> ()
     | Is_a_directory -> Path.rm_rf (Path.source dst)
     | Error e ->
       User_error.raise
         [ Pp.textf
             "Error promoting %s to %s"
             (Path.to_string correction_file)
             (Path.Source.to_string dst)
         ; Exn.pp e
         ]);
    let chmod = Path.Permissions.add Path.Permissions.write in
    Path.mkdir_p (Path.source (Path.Source.parent_exn dst));
    match Io.copy_file ~chmod ~src:correction_file ~dst:(Path.source dst) () with
    | () -> ()
    | exception Unix.Unix_error (e, _, _) ->
      User_error.raise
        [ Pp.textf "failed to promote %s" (Path.Source.to_string dst)
        ; Pp.text (Unix.error_message e)
        ]
  ;;

  let promote ({ src; staging; dst } as file) =
    let correction_file = correction_file file in
    let correction_exists = Fpath.exists (Path.to_string correction_file) in
    Console.print
      [ Pp.box
          ~indent:2
          (if correction_exists
           then
             Pp.textf
               "Promoting %s to %s."
               (Path.to_string_maybe_quoted (Path.build src))
               (Path.Source.to_string_maybe_quoted dst)
           else
             Pp.textf
               "%S Skipping promotion of %s to %s as the %s is missing."
               (Path.to_string correction_file)
               (Path.to_string_maybe_quoted (Path.build src))
               (Path.Source.to_string_maybe_quoted dst)
               (match staging with
                | None -> "file"
                | Some staging ->
                  Format.sprintf
                    "staging file (%s)"
                    (Path.to_string_maybe_quoted (Path.build staging))))
      ];
    if correction_exists then do_promote ~correction_file ~dst
  ;;
end

type what =
  [ `File
  | `Directory
  ]

let dyn_of_what =
  let open Dyn in
  function
  | `File -> variant "File" []
  | `Directory -> variant "Directory" []
;;

type op =
  | File of File.t
  | Delete of what * Path.Source.t

let dyn_of_op = function
  | File f -> Dyn.variant "File" [ File.to_dyn f ]
  | Delete (what, d) -> Dyn.variant "Delete" [ dyn_of_what what; Path.Source.to_dyn d ]
;;

type db = op list

let db : db ref = ref []
let clear_cache () = db := []
let () = Hooks.End_of_build.always clear_cache

let register_intermediate how ~source_file ~correction_file =
  let src = snd (Path.Build.split_sandbox_root correction_file) in
  Dune_trace.emit Promote (fun () ->
    Dune_trace.Event.Promote.register `Staged src source_file);
  let staging = File.in_staging_area source_file in
  let staging_dir = Path.Build.parent_exn staging in
  let mkdir_p () = Fpath.mkdir_p_strict (Path.Build.to_string staging_dir) in
  (match
     match mkdir_p () with
     | `Not_a_dir ->
       Path.rm_rf (Path.build staging_dir);
       mkdir_p ()
     | x -> x
   with
   | `Already_exists | `Created -> ()
   | `Not_a_dir ->
     Code_error.raise "dir was deleted" [ "staging_dir", Path.Build.to_dyn staging_dir ]);
  (match how with
   | `Move ->
     Unix.rename (Path.Build.to_string correction_file) (Path.Build.to_string staging)
   | `Copy ->
     Io.copy_file
       ~chmod:Path.Permissions.(add write)
       ~src:(Path.build correction_file)
       ~dst:(Path.build staging)
       ());
  db := File { src; staging = Some staging; dst = source_file } :: !db
;;

module P = Persistent.Make (struct
    type t = db

    let name = "TO-PROMOTE"
    let version = 4
    let to_dyn = Dyn.list dyn_of_op

    let test_example () =
      [ File
          { File.src = Path.Build.(relative root "foo")
          ; dst = Path.Source.of_string "bar"
          ; staging = Some Path.Build.(relative root "baz")
          }
      ; Delete (`File, Path.Source.of_string "foo")
      ; Delete (`Directory, Path.Source.of_string "foo")
      ]
    ;;
  end)

let db_file = Path.relative Path.build_dir ".to-promote"

let dump_db (db : db) =
  if Path.build_dir_exists ()
  then (
    match db with
    | [] ->
      if Fpath.exists (Path.to_string db_file)
      then Fpath.unlink_no_err (Path.to_string db_file)
    | l -> P.dump db_file l)
;;

let load_db () = Option.value ~default:[] (P.load db_file)

let group_by_targets db =
  List.map db ~f:(fun op ->
    match op with
    | Delete (_, f) -> f, op
    | File { File.src = _; staging = _; dst } -> dst, op)
  |> Path.Source.Map.of_list_multi
  (* Sort the list of possible sources for deterministic behavior *)
  |> Path.Source.Map.map ~f:(List.sort ~compare:Poly.compare)
;;

let promote_one dst srcs =
  match srcs with
  | [] -> assert false
  | op :: others ->
    (* We used to remove promoted files from the digest cache, to force Dune
       to redigest them on the next run. We did this because on OSX [mtime] is
       not precise enough and if a file is modified and promoted quickly, it
       looked like it hadn't changed even though it might have.

       aalekseyev: This is probably unnecessary now, depending on when
       [do_promote] runs (before or after [invalidate_cached_timestamps]).

       amokhov: I removed this logic. In the current state of the world, files
       in the build directory should be redigested automatically (plus we do
       not promote into the build directory anyway), and source digests should
       be correctly invalidated via [fs_memo]. If that doesn't happen, we
       should fix [fs_memo] instead of manually resetting the caches here. *)
    (match op with
     | File f -> File.promote f
     | Delete (`File, _) -> Fpath.unlink_exn (Path.Source.to_string dst)
     | Delete (`Directory, _) -> Fpath.rm_rf (Path.Source.to_string dst));
    List.iter others ~f:(fun op ->
      let path =
        match op with
        | File f -> Path.build f.src
        | Delete _ -> Path.source dst
      in
      Console.print
        [ Pp.textf " -> ignored %s." (Path.to_string_maybe_quoted path); Pp.space ])
;;

let do_promote_all db = group_by_targets db |> Path.Source.Map.iteri ~f:promote_one

let do_promote_these db files =
  let by_targets = group_by_targets db in
  let by_targets, missing =
    let files = Path.Source.Set.of_list files in
    Path.Source.Set.fold files ~init:(by_targets, []) ~f:(fun fn (map, missing) ->
      match Path.Source.Map.find map fn with
      | None -> map, fn :: missing
      | Some srcs ->
        promote_one fn srcs;
        Path.Source.Map.remove map fn, missing)
  in
  let remaining =
    Path.Source.Map.to_list by_targets |> List.concat_map ~f:(fun (_dst, ops) -> ops)
  in
  (* [group_by_targets] will sort all files, but the [fold] above reverses
     the list of missing files. Here we re-reverse it so it is sorted.
     CR-someday ElectreAAS: get rid of the fragile reversals and set
     conversions to do everything in one pass. *)
  let sorted_missing = List.rev missing in
  remaining, sorted_missing
;;

let do_promote db = function
  | Files_to_promote.All ->
    do_promote_all db;
    [], []
  | These files -> do_promote_these db files
;;

let finalize () =
  let db =
    match !Clflags.promote with
    | Some Automatically ->
      do_promote_all !db;
      []
    | Some Never | None -> !db
  in
  dump_db db
;;

(* Returns the list of files that were in [files_to_promote]
   but not present in the promotion database. *)
let promote_files_registered_in_last_run files_to_promote =
  let db = load_db () in
  let remaining, missing = do_promote db files_to_promote in
  dump_db remaining;
  missing
;;

type all =
  { present : File.t list
  ; missing : Path.Source.t list
  }

(** [partition_db db files_to_promote] splits [files_to_promote] into two lists
    - The files present in [db] as actual [File.t]s.
    - The files absent from [db] as [Path]s. *)
let partition_db (db : db) files_to_promote =
  let db =
    (* CR-soon rgrinberg: communicate deletions to the user *)
    List.filter_map db ~f:(function
      | File f -> Some f
      | Delete _ -> None)
  in
  let present, missing =
    match files_to_promote with
    | Files_to_promote.All -> db, []
    | These paths ->
      List.partition_map paths ~f:(fun path ->
        match List.find db ~f:(fun (f : File.t) -> Path.Source.equal f.dst path) with
        | Some file -> Left file
        | None -> Right path)
  in
  { present; missing }
;;

let register_delete what src = db := Delete (what, src) :: !db
