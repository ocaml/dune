open Import
open Jbuild_types

module Jbuilds = struct
  type script =
    { dir              : Path.t
    ; visible_packages : Package.t String_map.t
    ; closest_packages : Package.t list
    }

  type one =
    | Literal of Path.t * Stanza.t list
    | Script of script

  type t = one list

  let generated_jbuilds_dir = Path.(relative root) "_build/.jbuilds"

  let ensure_parent_dir_exists path =
    match Path.kind path with
    | Local path -> Path.Local.ensure_parent_directory_exists path
    | External _ -> ()

  let extract_requires str =
    List.fold_left (String.split str ~on:'\n') ~init:String_set.empty ~f:(fun acc line ->
      match Scanf.sscanf line "#require %S" (fun x -> x) with
      | exception _ -> acc
      | s ->
        String_set.union acc
          (String_set.of_list (String.split s ~on:',')))
    |> String_set.elements

  let create_plugin_wrapper (context : Context.t) ~exec_dir ~plugin ~wrapper ~target =
    let plugin = Path.to_string plugin in
    let plugin_contents = read_file plugin in
    with_file_out (Path.to_string wrapper) ~f:(fun oc ->
      Printf.fprintf oc {|
let () = Hashtbl.add Toploop.directive_table "require" (Toploop.Directive_string ignore)
module Jbuild_plugin = struct
  module V1 = struct
    let context       = %S
    let ocaml_version = %S

    let ocamlc_config =
      [ %s
      ]

    let send s =
      let oc = open_out_bin %S in
      output_string oc s;
      close_out oc
  end
end
# 1 %S
%s|}
        context.name
        context.version
        (String.concat ~sep:"\n      ; "
           (let longest = List.longest_map context.ocamlc_config ~f:fst in
            List.map context.ocamlc_config ~f:(fun (k, v) ->
                Printf.sprintf "%-*S , %S" (longest + 2) k v)))
        (Path.reach ~from:exec_dir target)
        plugin plugin_contents);
    extract_requires plugin_contents

  let eval jbuilds ~(context : Context.t) =
    let open Future in
    List.map jbuilds ~f:(function
      | Literal (path, stanzas) ->
        return (path, stanzas)
      | Script { dir
               ; visible_packages
               ; closest_packages
               } ->
        let file = Path.relative dir "jbuild" in
        let generated_jbuild =
          Path.append (Path.relative generated_jbuilds_dir context.name) file
        in
        let wrapper = Path.extend_basename generated_jbuild ~suffix:".ml" in
        ensure_parent_dir_exists generated_jbuild;
        let requires =
          create_plugin_wrapper context ~exec_dir:dir ~plugin:file ~wrapper
            ~target:generated_jbuild
        in
        let pkgs =
          List.map requires ~f:(Findlib.find_exn context.findlib
                                  ~required_by:[Utils.jbuild_name_in ~dir:dir])
          |> Findlib.closure
        in
        let includes =
          List.fold_left pkgs ~init:Path.Set.empty ~f:(fun acc pkg ->
            Path.Set.add pkg.Findlib.dir acc)
          |> Path.Set.elements
          |> List.concat_map ~f:(fun path ->
              [ "-I"; Path.to_string path ])
        in
        let cmas =
          List.concat_map pkgs ~f:(fun pkg -> pkg.archives.byte)
        in
        let args =
          List.concat
            [ [ "-I"; "+compiler-libs" ]
            ; includes
            ; cmas
            ; [ Path.reach ~from:dir wrapper ]
            ]
        in
        (* CR-someday jdimino: if we want to allow plugins to use findlib:
           {[
             let args =
               match context.toplevel_path with
               | None -> args
               | Some path -> "-I" :: Path.reach ~from:dir path :: args
             in
           ]}
        *)
        Future.run Strict ~dir:(Path.to_string dir) ~env:context.env
          (Path.to_string context.ocaml)
          args
        >>= fun () ->
        let sexps = Sexp_load.many (Path.to_string generated_jbuild) in
        return (dir, Stanzas.parse sexps ~dir ~visible_packages ~closest_packages))
    |> Future.all
end

type conf =
  { file_tree : File_tree.t
  ; tree      : Alias.tree
  ; jbuilds   : Jbuilds.t
  ; packages  : Package.t String_map.t
  }

let load ~dir ~visible_packages ~closest_packages =
  let file = Path.relative dir "jbuild" in
  match Sexp_load.many_or_ocaml_script (Path.to_string file) with
  | Sexps sexps ->
    Jbuilds.Literal (dir, Stanzas.parse sexps ~dir ~visible_packages ~closest_packages)
  | Ocaml_script ->
    Script
      { dir
      ; visible_packages
      ; closest_packages
      }

let load () =
  let ftree = File_tree.load Path.root in
  let packages =
    File_tree.fold ftree ~init:[] ~f:(fun dir acc ->
      let path = File_tree.Dir.path dir in
      String_set.fold (File_tree.Dir.files dir) ~init:acc ~f:(fun fn acc ->
        match Filename.split_ext fn with
        | Some (pkg, ".opam") when pkg <> "" ->
          let version_from_opam_file =
            let lines = lines_of_file (Path.relative path fn |> Path.to_string) in
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
  let rec walk dir jbuilds visible_packages closest_packages =
    let path = File_tree.Dir.path dir in
    let files = File_tree.Dir.files dir in
    let sub_dirs = File_tree.Dir.sub_dirs dir in
    let visible_packages, closest_packages =
      match Path.Map.find path packages_per_dir with
      | None -> (visible_packages, closest_packages)
      | Some pkgs ->
        (List.fold_left pkgs ~init:visible_packages ~f:(fun acc pkg ->
           String_map.add acc ~key:pkg.Package.name ~data:pkg),
         pkgs)
    in
    let jbuilds =
      if String_set.mem "jbuild" files then
        let jbuild = load ~dir:path ~visible_packages ~closest_packages in
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
          let child, jbuilds = walk dir jbuilds visible_packages closest_packages in
          (child :: children, jbuilds))
    in
    (Alias.Node (path, children), jbuilds)
  in
  let root = File_tree.root ftree in
  let tree, jbuilds = walk root [] String_map.empty [] in
  { file_tree = ftree
  ; tree
  ; jbuilds
  ; packages
  }
