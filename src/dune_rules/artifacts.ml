open Import
open Memo.O

let bin_dir_basename = ".bin"
let local_bin p = Path.Build.relative p bin_dir_basename

type origin =
  { binding : File_binding.Unexpanded.t
  ; dir : Path.Build.t
  }

type local_bins = (Path.Build.t * origin option) Filename.Map.t

type t =
  { context : Context.t
  ; (* Mapping from executable names to their actual path in the workspace.
       The keys are the executable names without the .exe, even on Windows.
       Enumerating binaries from install stanzas may involve expanding globs,
       but the artifacts database is depended on by the logic which expands
       globs. The computation of this field is deferred to break the cycle. *)
    local_bins : local_bins Memo.Lazy.t
  }

let force { local_bins; _ } =
  let+ (_ : local_bins) = Memo.Lazy.force local_bins in
  ()
;;

let analyze_binary t name =
  match Filename.is_relative name with
  | false -> Memo.return (Some (Path.of_filename_relative_to_initial_cwd name, None))
  | true ->
    let* local_bins = Memo.Lazy.force t.local_bins in
    (match Filename.Map.find local_bins name with
     | Some (path, origin) -> Memo.return (Some (Path.build path, origin))
     | None ->
       let+ res = Context.which t.context name in
       Option.map res ~f:(fun res -> res, None))
;;

let binary t ?hint ~loc name =
  analyze_binary t name
  >>| function
  | Some (path, _) -> Ok path
  | None ->
    let context = Context.name t.context in
    Error (Action.Prog.Not_found.create ~program:name ?hint ~context ~loc ())
;;

let binary_with_origin t ?hint ~loc name =
  analyze_binary t name
  >>| function
  | Some (path, origin) ->
    Ok
      (match origin with
       | None -> `External path
       | Some origin -> `Origin origin)
  | None ->
    let context = Context.name t.context in
    Error (Action.Prog.Not_found.create ~program:name ?hint ~context ~loc ())
;;

let binary_available t name =
  analyze_binary t name
  >>= function
  | None -> Memo.return false
  | Some (path, _) ->
    (match path with
     | External e -> Fs_memo.file_exists @@ External e
     | In_source_tree e -> Fs_memo.file_exists @@ In_source_dir e
     | In_build_dir _ -> Memo.return true)
;;

let add_binaries t ~dir l =
  let local_bins =
    Memo.lazy_ ~name:"Artifacts.Bin.add_binaries" (fun () ->
      let+ local_bins = Memo.Lazy.force t.local_bins in
      List.fold_left l ~init:local_bins ~f:(fun acc fb ->
        let path = File_binding.Expanded.dst_path fb ~dir:(local_bin dir) in
        Filename.Map.set acc (Path.Build.basename path) (path, None)))
  in
  { t with local_bins }
;;

let create =
  let drop_suffix name =
    if Sys.win32
    then Option.value ~default:name (String.drop_suffix name ~suffix:".exe")
    else name
  in
  fun (context : Context.t) ~local_bins ->
    let local_bins =
      Memo.lazy_ (fun () ->
        let+ local_bins = Memo.Lazy.force local_bins in
        Path.Build.Map.foldi
          local_bins
          ~init:Filename.Map.empty
          ~f:(fun path origin acc ->
            let name = Path.Build.basename path in
            let key = drop_suffix name in
            Filename.Map.set acc key (path, Some origin)))
    in
    { context; local_bins }
;;
