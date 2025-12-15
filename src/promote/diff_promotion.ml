open Import

let staging_area = Path.Build.relative Path.Build.root ".promotion-staging"

module File = struct
  type t =
    { src : Path.Build.t
    ; staging : Path.Build.t option
    ; dst : Path.Source.t
    }

  let source t = t.dst

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

  let db : t list ref = ref []

  let register_dep ~source_file ~correction_file =
    db
    := { src = snd (Path.Build.split_sandbox_root correction_file)
       ; staging = None
       ; dst = source_file
       }
       :: !db
  ;;

  let register_intermediate ~source_file ~correction_file =
    let staging = in_staging_area source_file in
    Path.mkdir_p (Path.build (Option.value_exn (Path.Build.parent staging)));
    Unix.rename (Path.Build.to_string correction_file) (Path.Build.to_string staging);
    let src = snd (Path.Build.split_sandbox_root correction_file) in
    db := { src; staging = Some staging; dst = source_file } :: !db
  ;;

  let do_promote ~correction_file ~dst =
    Fpath.unlink_no_err (Path.Source.to_string dst);
    let chmod = Path.Permissions.add Path.Permissions.write in
    match Io.copy_file ~chmod ~src:correction_file ~dst:(Path.source dst) () with
    | () -> ()
    | exception Unix.Unix_error (e, _, _) ->
      User_error.raise
        [ Pp.textf "failed to promote %s" (Path.Source.to_string dst)
        ; Pp.text (Unix.error_message e)
        ]
  ;;

  let correction_file { src; staging; _ } = Path.build (Option.value staging ~default:src)

  let promote ({ src; staging; dst } as file) =
    let correction_file = correction_file file in
    let correction_exists = Path.exists correction_file in
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
               "Skipping promotion of %s to %s as the %s is missing."
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

let clear_cache () = File.db := []
let () = Hooks.End_of_build.always clear_cache

module P = Persistent.Make (struct
    type t = File.t list

    let name = "TO-PROMOTE"
    let version = 3
    let to_dyn = Dyn.list File.to_dyn

    let test_example () =
      [ { File.src = Path.Build.(relative root "foo")
        ; dst = Path.Source.of_string "bar"
        ; staging = Some Path.Build.(relative root "baz")
        }
      ]
    ;;
  end)

let db_file = Path.relative Path.build_dir ".to-promote"

let dump_db db =
  if Path.build_dir_exists ()
  then (
    match db with
    | [] -> if Path.exists db_file then Fpath.unlink_no_err (Path.to_string db_file)
    | l -> P.dump db_file l)
;;

let load_db () = Option.value ~default:[] (P.load db_file)

let group_by_targets db =
  List.map db ~f:(fun { File.src; staging; dst } -> dst, (src, staging))
  |> Path.Source.Map.of_list_multi
  (* Sort the list of possible sources for deterministic behavior *)
  |> Path.Source.Map.map
       ~f:(List.sort ~compare:(fun (x, _) (y, _) -> Path.Build.compare x y))
;;

let promote_one dst srcs =
  match srcs with
  | [] -> assert false
  | (src, staging) :: others ->
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
    File.promote { src; staging; dst };
    List.iter others ~f:(fun (path, _staging) ->
      Console.print
        [ Pp.textf " -> ignored %s." (Path.to_string_maybe_quoted (Path.build path))
        ; Pp.space
        ])
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
    Path.Source.Map.to_list by_targets
    |> List.concat_map ~f:(fun (dst, srcs) ->
      List.map srcs ~f:(fun (src, staging) -> { File.src; staging; dst }))
  in
  (* [group_by_targets] will sort all files, but the [fold] above reverses
     the list of missing files. Here we re-reverse it so it is sorted.
     CR-someday ElectreAAS: get rid of the fragile reversals and set
     conversions to do everything in one pass. *)
  let sorted_missing = List.rev missing in
  remaining, sorted_missing
;;

let do_promote db = function
  | Dune_rpc_private.Files_to_promote.All ->
    do_promote_all db;
    [], []
  | These files -> do_promote_these db files
;;

let finalize () =
  let db =
    match !Dune_engine.Clflags.promote with
    | Some Automatically ->
      do_promote_all !File.db;
      []
    | Some Never | None -> !File.db
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

let diff_for_file (file : File.t) =
  let msg = User_message.Annots.empty in
  let original = Path.source file.dst in
  let correction = File.correction_file file in
  Print_diff.get msg original correction
;;

(** [partition_db db files_to_promote] splits [files_to_promote] into two lists
    - The files present in [db] as actual [File.t]s.
    - The files absent from [db] as [Path]s. *)
let partition_db db files_to_promote =
  match files_to_promote with
  | Dune_rpc_private.Files_to_promote.All -> db, []
  | These paths ->
    List.partition_map paths ~f:(fun path ->
      let res = List.find db ~f:(fun (f : File.t) -> Path.Source.equal f.dst path) in
      match res with
      | Some file -> Left file
      | None -> Right path)
;;

let sort_for_display db files_to_promote =
  let open Fiber.O in
  let files, missing = partition_db db files_to_promote in
  let+ diff_opts =
    Fiber.parallel_map files ~f:(fun file ->
      let+ diff_opt = diff_for_file file in
      match diff_opt with
      | Ok diff -> Some (file, diff)
      | Error _ -> None)
  in
  let sorted_diffs =
    diff_opts
    |> List.filter_opt
    |> List.sort ~compare:(fun (file, _) (file', _) -> File.compare file file')
  in
  let sorted_missing = List.sort missing ~compare:Path.Source.compare in
  sorted_diffs, sorted_missing
;;

let missing ~db files_to_promote =
  let open Fiber.O in
  let+ _diffs, missing = sort_for_display db files_to_promote in
  missing
;;

let display_diffs ~db files_to_promote =
  let open Fiber.O in
  let+ diffs, _missing = sort_for_display db files_to_promote in
  List.iter diffs ~f:(fun (_file, diff) -> Print_diff.Diff.print diff)
;;

let display_files ~db files_to_promote =
  let open Fiber.O in
  let+ diffs, _missing = sort_for_display db files_to_promote in
  List.iter diffs ~f:(fun (file, _diff) ->
    Console.printf "%s" (File.source file |> Path.Source.to_string))
;;

let display_corrected_contents ~db files_to_promote =
  let files, _missing = partition_db db files_to_promote in
  List.iter files ~f:(fun file ->
    let correction_file = File.correction_file file in
    if Path.exists correction_file
    then (
      let contents = Io.read_file correction_file in
      Console.printf "%s" contents)
    else
      User_warning.emit
        [ Pp.textf
            "Corrected file does not exist for %s."
            (File.source file |> Path.Source.to_string_maybe_quoted)
        ])
;;
