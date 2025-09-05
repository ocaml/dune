open Import
open Memo.O

type mld =
  { path : Path.Build.t
  ; in_doc : Path.Local.t
  }

module T = struct
  type t = mld

  let to_dyn { path; in_doc } =
    Dyn.Tuple [ Path.Build.to_dyn path; Path.Local.to_dyn in_doc ]
  ;;

  let compare x y =
    let open Ordering.O in
    let= () = Path.Build.compare x.path y.path in
    Path.Local.compare x.in_doc y.in_doc
  ;;
end

module Map = Map.Make (T)
module Set = Set.Make (T) (Map)

let of_file_bindings fbs =
  List.map fbs ~f:(fun file_binding ->
    let path = File_binding.Expanded.src file_binding in
    let in_doc =
      match File_binding.Expanded.dst file_binding with
      | None -> Path.Local.of_string (Path.Build.basename path)
      | Some in_doc ->
        let loc = File_binding.Expanded.src_loc file_binding in
        Path.Local.parse_string_exn ~loc in_doc
    in
    { path; in_doc })
;;

let from_mld_files mlds (doc : Documentation.t) dir =
  Ordered_set_lang.Unordered_string.eval
    doc.mld_files
    ~standard:mlds
    ~key:Fun.id
    ~parse:(fun ~loc s ->
      match Filename.Map.find mlds (s ^ ".mld") with
      | Some s -> s
      | None ->
        User_error.raise
          ~loc
          [ Pp.textf
              "%s doesn't exist in %s"
              s
              (Path.to_string_maybe_quoted
                 (Path.drop_optional_build_context (Path.build dir)))
          ])
  |> Filename.Map.map ~f:(fun in_doc ->
    let path = Path.Build.relative dir in_doc in
    let in_doc = Path.Local.of_string in_doc in
    { path; in_doc })
  |> Filename.Map.values
;;

let build_mlds_map stanzas ~dir ~files expander =
  let mlds =
    lazy
      (Filename.Set.fold files ~init:Filename.Map.empty ~f:(fun fn acc ->
         match String.rsplit2 fn ~on:'.' with
         | Some (_, "mld") -> Filename.Map.set acc fn fn
         | _ -> acc))
  in
  Dune_file.find_stanzas stanzas Documentation.key
  >>= Memo.parallel_map ~f:(fun (doc : Documentation.t) ->
    let from_mld_files = from_mld_files (Lazy.force mlds) doc dir in
    let+ from_files =
      let expand = Expander.No_deps.expand expander ~mode:Single in
      Install_entry.File.to_file_bindings_expanded doc.files ~expand ~dir
      >>| of_file_bindings
    in
    let mlds = from_mld_files @ from_files in
    let mlds = mlds |> Set.of_list |> Set.to_list in
    doc, mlds)
;;
