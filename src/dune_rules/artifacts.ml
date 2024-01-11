open Import
open Memo.O

let bin_dir_basename = ".bin"
let local_bin p = Path.Build.relative p bin_dir_basename

type origin =
  { binding : File_binding.Unexpanded.t
  ; dir : Path.Build.t
  ; dst : Path.Local.t
  }

type where =
  | Install_dir
  | Original_path

type path =
  | Resolved of Path.Build.t
  | Origin of origin

type local_bins = path Filename.Map.t

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

let expand = Fdecl.create Dyn.opaque

let analyze_binary t name =
  match Filename.is_relative name with
  | false -> Memo.return (`Resolved (Path.of_filename_relative_to_initial_cwd name))
  | true ->
    let* local_bins = Memo.Lazy.force t.local_bins in
    (match Filename.Map.find local_bins name with
     | Some (Resolved p) -> Memo.return (`Resolved (Path.build p))
     | Some (Origin o) -> Memo.return (`Origin o)
     | None ->
       Context.which t.context name
       >>| (function
        | None -> `None
        | Some path -> `Resolved path))
;;

let binary t ?hint ?(where = Install_dir) ~loc name =
  analyze_binary t name
  >>= function
  | `Resolved path -> Memo.return @@ Ok path
  | `None ->
    let context = Context.name t.context in
    Memo.return
    @@ Error (Action.Prog.Not_found.create ~program:name ?hint ~context ~loc ())
  | `Origin { dir; binding; dst } ->
    (match where with
     | Install_dir ->
       let install_dir = Install.Context.bin_dir ~context:(Context.name t.context) in
       Memo.return @@ Ok (Path.build @@ Path.Build.append_local install_dir dst)
     | Original_path ->
       let+ expanded =
         File_binding.Unexpanded.expand
           binding
           ~dir
           ~f:(Fdecl.get expand ~context:t.context ~dir)
       in
       let src = File_binding.Expanded.src expanded in
       Ok (Path.build src))
;;

let binary_available t name =
  analyze_binary t name
  >>| function
  | `None -> false
  | `Resolved _ | `Origin _ -> true
;;

let add_binaries t ~dir l =
  let local_bins =
    Memo.lazy_ ~name:"Artifacts.Bin.add_binaries" (fun () ->
      let+ local_bins = Memo.Lazy.force t.local_bins in
      List.fold_left l ~init:local_bins ~f:(fun acc fb ->
        let path = File_binding.Expanded.dst_path fb ~dir:(local_bin dir) in
        Filename.Map.set acc (Path.Build.basename path) (Resolved path)))
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
        Filename.Map.foldi local_bins ~init:Filename.Map.empty ~f:(fun name origin acc ->
          let key = drop_suffix name in
          Filename.Map.set acc key (Origin origin)))
    in
    { context; local_bins }
;;
