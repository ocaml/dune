open Import
open Jbuild_types

module Jbuild = struct
  type t =
    { path             : Path.t
    ; version          : Jbuild_types.Jbuilder_version.t
    ; sexps            : Sexp.Ast.t list
    ; visible_packages : Package.t String_map.t
    }
end

type conf =
  { file_tree : File_tree.t
  ; tree      : Alias.tree
  ; jbuilds   : Jbuild.t list
  ; packages  : Package.t String_map.t
  }

let load ~dir ~visible_packages ~version =
  let sexps = Sexp_load.many (Path.relative dir "jbuild" |> Path.to_string) in
  let versions, sexps =
    List.partition_map sexps ~f:(function
      | List (loc, [Atom (_, ("jbuilder_version" | "Jbuilder_version")); ver]) ->
        Inl (Jbuilder_version.t ver, loc)
      | sexp -> Inr sexp)
  in
  let version =
    match versions with
    | [] -> version
    | [(v, _)] -> v
    | _ :: (_, loc) :: _ ->
      Loc.fail loc "jbuilder_version specified too many times"
  in
  { Jbuild.
    path = dir
  ; version
  ; sexps
  ; visible_packages
  }

let load () =
  let ftree = File_tree.load Path.root in
  let packages =
    File_tree.fold ftree ~init:[] ~f:(fun dir acc ->
      let path = File_tree.Dir.path dir in
      String_set.fold (File_tree.Dir.files dir) ~init:acc ~f:(fun fn acc ->
        match Filename.split_ext fn with
        | Some (pkg, ".opam") ->
          let version_from_opam_file =
            let lines = lines_of_file fn in
            List.find_map lines ~f:(fun s ->
              try
                Scanf.sscanf s "version: %S" (fun x -> Some x)
              with _ ->
                None)
          in
          (pkg,
           { Package. name = pkg
           ; path
           ; version_from_opam_file
           }) :: acc
        | _ -> acc))
    |> String_map.of_alist_multi
    |> String_map.mapi ~f:(fun name pkgs ->
      match pkgs with
      | [pkg] -> pkg
      | _ ->
        die "Too many opam files for package %S:\n%s"
          name
          (String.concat ~sep:"\n"
             (List.map pkgs ~f:(fun pkg ->
                sprintf "- %s.opam" (Path.to_string pkg.Package.path)))))
  in
  let packages_per_dir =
    String_map.values packages
    |> List.map ~f:(fun pkg -> (pkg.Package.path, pkg))
    |> Path.Map.of_alist_multi
  in
  let rec walk dir jbuilds visible_packages version =
    let path = File_tree.Dir.path dir in
    let files = File_tree.Dir.files dir in
    let sub_dirs = File_tree.Dir.sub_dirs dir in
    let visible_packages =
      match Path.Map.find path packages_per_dir with
      | None -> visible_packages
      | Some pkgs ->
        List.fold_left pkgs ~init:visible_packages ~f:(fun acc pkg ->
          String_map.add acc ~key:pkg.Package.name ~data:pkg)
    in
    let version, jbuilds =
      if String_set.mem "jbuild" files then
        let jbuild = load ~dir:path ~visible_packages ~version in
        (jbuild.version, jbuild :: jbuilds)
      else
        (version, jbuilds)
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
    let children, jbuilds =
      String_map.fold sub_dirs ~init:([], jbuilds)
        ~f:(fun ~key:_ ~data:dir (children, jbuilds) ->
          let child, jbuilds = walk dir jbuilds visible_packages version in
          (child :: children, jbuilds))
    in
    (Alias.Node (path, children), jbuilds)
  in
  let root = File_tree.root ftree in
  let tree, jbuilds = walk root [] String_map.empty Jbuilder_version.latest_stable in
  { file_tree = ftree
  ; tree
  ; jbuilds
  ; packages
  }
