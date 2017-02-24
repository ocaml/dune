open Import
open Jbuild_types

type conf =
  { file_tree : File_tree.t
  ; tree      : Alias.tree
  ; stanzas   : (Path.t * Jbuild_types.Stanza.t list) list
  ; packages  : Path.t String_map.t
  }

let load ~dir ~visible_packages ~version =
  let version, stanzas =
    Sexp_load.many (Path.relative dir "jbuild" |> Path.to_string)
      (fun sexps ->
         let versions, sexps =
           List.partition_map sexps ~f:(function
             | List [Atom ("jbuilder_version" | "Jbuilder_version"); ver] as sexp ->
               Inl (Jbuilder_version.t ver, sexp)
             | sexp -> Inr sexp)
         in
         let version =
           match versions with
           | [] -> version
           | [(v, _)] -> v
           | _ :: (_, sexp) :: _ ->
             of_sexp_error sexp "jbuilder_version specified too many times"
         in
         (version, List.filter_map sexps ~f:(Stanza.select version)))
  in
  let stanzas = Stanza.resolve_packages stanzas ~dir ~visible_packages in
  (version, stanzas)

let load () =
  let ftree = File_tree.load Path.root in
  let packages =
    File_tree.fold ftree ~init:[] ~f:(fun dir acc ->
      let path = File_tree.Dir.path dir in
      String_set.fold (File_tree.Dir.files dir) ~init:acc ~f:(fun fn acc ->
        match Filename.split_ext fn with
        | Some (pkg, ".opam") -> (pkg, path) :: acc
        | _ -> acc))
    |> String_map.of_alist_multi
    |> String_map.mapi ~f:(fun pkg dirs ->
      match dirs with
      | [dir] -> dir
      | _ ->
        die "Too many opam files for package %S:\n%s"
          pkg
          (String.concat ~sep:"\n"
             (List.map dirs ~f:(fun dir ->
                sprintf "- %s.opam" (Path.to_string dir)))))
  in
  let packages_per_dir =
    String_map.bindings packages
    |> List.map ~f:(fun (pkg, path) -> (path, pkg))
    |> Path.Map.of_alist_multi
  in
  let rec walk dir stanzas visible_packages version =
    let path = File_tree.Dir.path dir in
    let files = File_tree.Dir.files dir in
    let sub_dirs = File_tree.Dir.sub_dirs dir in
    let visible_packages =
      match Path.Map.find path packages_per_dir with
      | None -> visible_packages
      | Some pkgs ->
        List.fold_left pkgs ~init:visible_packages ~f:(fun acc pkg ->
          String_map.add acc ~key:pkg ~data:path)
    in
    let version, stanzas =
      if String_set.mem "jbuild" files then
        let version, stanzas_here = load ~dir:path ~visible_packages ~version in
        (version, (path, stanzas_here) :: stanzas)
      else
        (version, stanzas)
    in
    let sub_dirs =
      if String_set.mem "jbuild-ignore" files then
        let ignore_set =
          String_set.of_list
            (lines_of_file (Path.to_string (Path.relative path "jbuild-ignore")))
        in
        String_map.filter sub_dirs ~f:(fun fn _ ->
          not (String_set.mem fn ignore_set))
      else
        sub_dirs
    in
    let children, stanzas =
      String_map.fold sub_dirs ~init:([], stanzas)
        ~f:(fun ~key:_ ~data:dir (children, stanzas) ->
          let child, stanzas = walk dir stanzas visible_packages version in
          (child :: children, stanzas))
    in
    (Alias.Node (path, children), stanzas)
  in
  let root = File_tree.root ftree in
  let tree, stanzas = walk root [] String_map.empty Jbuilder_version.latest_stable in
  { file_tree = ftree
  ; tree
  ; stanzas
  ; packages
  }
