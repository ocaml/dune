open Import
open Jbuild_types

module Jbuilds = struct
  type script =
    { dir              : Path.t
    ; visible_packages : Package.t String_map.t
    }

  type one =
    | Literal of Path.t * Stanza.t list
    | Script of script

  type t = one list

  let generated_jbuilds_dir = Path.(relative root) "_build/.jbuilds"
  let contexts_files_dir = Path.(relative root) "_build/.contexts"

  let ensure_parent_dir_exists path =
    match Path.kind path with
    | Local path -> Path.Local.ensure_parent_directory_exists path
    | External _ -> ()

  let create_context_file (context : Context.t) =
    let file = Path.relative contexts_files_dir (context.name ^ ".ml") in
    ensure_parent_dir_exists file;
    with_file_out (Path.to_string file) ~f:(fun oc ->
      Printf.fprintf oc {|
module Jbuild_plugin = struct
  module V1 = struct
    let context       = %S
    let ocaml_version = %S

    let ocamlc_config =
      [ %s
      ]

    let send s =
      let oc = open_out_bin Sys.argv.(1) in
      output_string oc s;
      close_out oc
  end
end
|}
        context.name
        context.version
        (String.concat ~sep:"\n      ; "
           (let longest = List.longest_map context.ocamlc_config ~f:fst in
            List.map context.ocamlc_config ~f:(fun (k, v) ->
              Printf.sprintf "%-*S , %S" (longest + 2) k v))));
    file

  let eval jbuilds ~(context : Context.t) =
    let open Future in
    let context_files = Hashtbl.create 8 in
    List.map jbuilds ~f:(function
      | Literal (path, stanzas) ->
        return (path, stanzas)
      | Script { dir
               ; visible_packages
               } ->
        let file = Path.relative dir "jbuild" in
        let generated_jbuild =
          Path.append (Path.relative generated_jbuilds_dir context.name) file
        in
        let wrapper = Path.extend_basename generated_jbuild ~suffix:".ml" in
        ensure_parent_dir_exists generated_jbuild;
        let context_file, context_file_contents =
          Hashtbl.find_or_add context_files context.name ~f:(fun () ->
            let file = create_context_file context in
            (file, read_file (Path.to_string file)))
        in
        Printf.ksprintf (write_file (Path.to_string wrapper))
          "# 1 %S\n\
           %s\n\
           # 1 %S\n\
           %s"
          (Path.to_string context_file)
          context_file_contents
          (Path.to_string file)
          (read_file (Path.to_string file));
        Future.run Strict ~dir:(Path.to_string dir) ~env:context.env
          (Path.to_string context.Context.ocaml)
          [ Path.reach ~from:dir wrapper
          ; Path.reach ~from:dir generated_jbuild
          ]
        >>= fun () ->
        let sexps = Sexp_load.many (Path.to_string generated_jbuild) in
        return (dir, Stanzas.parse sexps ~dir ~visible_packages))
    |> Future.all
end

type conf =
  { file_tree : File_tree.t
  ; tree      : Alias.tree
  ; jbuilds   : Jbuilds.t
  ; packages  : Package.t String_map.t
  }

let load ~dir ~visible_packages =
  let file = Path.relative dir "jbuild" in
  match Sexp_load.many_or_ocaml_script (Path.to_string file) with
  | Sexps sexps ->
    Jbuilds.Literal (dir, Stanzas.parse sexps ~dir ~visible_packages)
  | Ocaml_script ->
    Script
      { dir
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
  let rec walk dir jbuilds visible_packages =
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
    let jbuilds =
      if String_set.mem "jbuild" files then
        let jbuild = load ~dir:path ~visible_packages in
        jbuild :: jbuilds
      else
        jbuilds
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
          let child, jbuilds = walk dir jbuilds visible_packages in
          (child :: children, jbuilds))
    in
    (Alias.Node (path, children), jbuilds)
  in
  let root = File_tree.root ftree in
  let tree, jbuilds = walk root [] String_map.empty in
  { file_tree = ftree
  ; tree
  ; jbuilds
  ; packages
  }
