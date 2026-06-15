open Import
open Memo.O

let bin_dir_basename = Filename.bin_dir_basename
let local_bin p = Path.Build.relative_fname p bin_dir_basename

type origin =
  { binding : File_binding.Unexpanded.t
  ; dir : Path.Build.t
  ; dst : Path.Local.t
  ; enabled_if : bool Memo.t
  ; package : Package.Name.t option
  }

type where =
  | Install_dir
  | Original_path

type path =
  | Resolved of
      { binding : File_binding.Expanded.t
      ; path : Path.Build.t
      }
  | Origin of origin list

type local_bins = path Filename.Map.t

type t =
  { context : Context.t
  ; (* Mapping from binary lookup names to their definitions. On Windows, a
       trailing [.exe] is removed from lookup names, while [Origin.dst] retains
       the actual install filename.
       Enumerating binaries from install stanzas may involve expanding globs,
       but the artifacts database is depended on by the logic which expands
       globs. The computation of this field is deferred to break the cycle. *)
    local_bins : local_bins Memo.Lazy.t
  ; (* Package names of dependencies of the dir's owning package. *)
    owning_package_deps : Package.Name.Set.t option
  }

let force { local_bins; _ } =
  let+ (_ : local_bins) = Memo.Lazy.force local_bins in
  ()
;;

let local_binaries { local_bins; _ } =
  let+ local_bins = Memo.Lazy.force local_bins in
  List.filter_map (Filename.Map.to_list local_bins) ~f:(function
    | _, Resolved p -> Some p.binding
    | _, Origin _origins -> None)
;;

let analyze_binary t ~dir name =
  match Filename.analyze_program_name name with
  | Absolute -> Memo.return (`Resolved (Path.of_filename_relative_to_initial_cwd name))
  | (In_path | Relative_to_current_dir) as kind ->
    let* local_bins = Memo.Lazy.force t.local_bins in
    let lookup_name =
      match kind with
      | In_path -> Filename.of_string name
      | Relative_to_current_dir -> Path.Build.relative dir name |> Path.Build.basename_opt
      | Absolute ->
        Code_error.raise
          "Artifacts.analyze_binary: unexpected absolute program name"
          [ "name", Dyn.string name ]
    in
    let which () =
      match lookup_name with
      | None -> Memo.return `None
      | Some lookup_name ->
        Context.which t.context ~narrow_to_packages:t.owning_package_deps lookup_name
        >>| (function
         | None -> `None
         | Some path -> `Resolved path)
    in
    (match Option.bind lookup_name ~f:(Filename.Map.find local_bins) with
     | Some (Resolved p) -> Memo.return (`Resolved (Path.build p.path))
     | None -> which ()
     | Some (Origin origins) ->
       Memo.parallel_map origins ~f:(fun origin ->
         origin.enabled_if
         >>| function
         | true -> Some origin
         | false -> None)
       >>| List.filter_opt
       >>= (function
        | [] -> which ()
        | [ x ] -> Memo.return (`Origin x)
        | x :: rest ->
          let loc x = File_binding.Unexpanded.loc x.binding in
          User_error.raise
            ~loc:(loc x)
            [ Pp.textf
                "binary %S is available from more than one definition. It is also \
                 available in:"
                name
            ; Pp.enumerate rest ~f:(fun x -> Pp.verbatim (Loc.to_file_colon_line (loc x)))
            ]))
;;

let binary t ?hint ?(where = Original_path) ~dir ~loc name =
  analyze_binary t ~dir name
  >>= function
  | `Resolved path -> Memo.return @@ Ok path
  | `None ->
    let hint =
      match t.owning_package_deps, hint with
      | Some _, None ->
        Some
          (sprintf
             "%S is not provided by any dependency of this directory's package. Add a \
              dependency on the package that provides it."
             name)
      | _ -> hint
    in
    let context = Context.name t.context in
    Memo.return
    @@ Error
         (Action.Prog.Not_found.create
            ~program:(Filename.of_string_exn name)
            ?hint
            ~context
            ~loc
            ())
  | `Origin { dir; binding; dst; enabled_if = _; package = _ } ->
    (match where with
     | Install_dir ->
       let install_dir = Install.Context.bin_dir ~context:(Context.name t.context) in
       Memo.return @@ Ok (Path.build @@ Path.Build.append_local install_dir dst)
     | Original_path ->
       let+ expanded =
         let* expander = Expander0.get ~dir in
         File_binding_expand.expand
           binding
           ~dir
           ~f:(Expander0.expand_str_and_build_deps expander)
       in
       let src = File_binding.Expanded.src expanded in
       Ok (Path.build src))
;;

let local_binary_install_name t ~dir name =
  analyze_binary t ~dir name
  >>| function
  | `Origin { package = Some _; dst; _ } -> Some (Path.Local.basename dst)
  | `Origin { package = None; _ } | `Resolved _ | `None -> None
;;

let binary_available t ~dir name =
  analyze_binary t ~dir name
  >>| function
  | `None -> false
  | `Resolved _ | `Origin _ -> true
;;

let add_binaries t ~dir l =
  let local_bins =
    Memo.lazy_ ~name:"Artifacts.Bin.add_binaries" (fun () ->
      let+ local_bins = Memo.Lazy.force t.local_bins in
      List.fold_left l ~init:local_bins ~f:(fun acc binding ->
        let path = File_binding.Expanded.dst_path binding ~dir:(local_bin dir) in
        Filename.Map.set acc (Path.Build.basename path) (Resolved { binding; path })))
  in
  { t with local_bins }
;;

let set_owning_package_deps t ~owning_package_deps = { t with owning_package_deps }

let create =
  fun (context : Context.t)
    ~(local_bins : origin Appendable_list.t Filename.Map.t Memo.Lazy.t) ->
  let local_bins =
    Memo.lazy_ (fun () ->
      let+ local_bins = Memo.Lazy.force local_bins in
      Filename.Map.to_list_map local_bins ~f:(fun name sources ->
        ( Filename.of_string_exn (Bin.strip_exe (Filename.to_string name))
        , Origin (Appendable_list.to_list sources) ))
      |> Filename.Map.of_list_exn)
  in
  { context; local_bins; owning_package_deps = None }
;;
