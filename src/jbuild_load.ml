open Import
open Jbuild_types

type conf =
  { file_tree : File_tree.t
  ; tree      : Alias.tree
  ; stanzas   : (Path.t * Jbuild_types.Stanza.t list) list
  ; packages  : Path.t String_map.t
  }

let load fn ~dir = (dir, Sexp_load.many fn Stanza.t)

let load () =
  let rec walk dir stanzas packages =
    let path = File_tree.Dir.path dir in
    let files = File_tree.Dir.files dir in
    let sub_dirs = File_tree.Dir.sub_dirs dir in
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
    let children, stanzas, packages =
      String_map.fold sub_dirs ~init:([], stanzas, [])
        ~f:(fun ~key:_ ~data:dir (children, stanzas, packages) ->
          let child, stanzas, packages = walk dir stanzas packages in
          (child :: children, stanzas, packages))
    in
    let stanzas =
      if String_set.mem "jbuild" files then
        load (Path.to_string (Path.relative path "jbuild")) ~dir:path :: stanzas
      else
        stanzas
    in
    let packages =
      String_set.fold files ~init:packages ~f:(fun fn acc ->
        match Filename.split_ext fn with
        | Some (pkg, ".opam") -> (pkg, path) :: acc
        | _ -> acc)
    in
    (Alias.Node (path, children), stanzas, packages)
  in
  let ftree = File_tree.load Path.root in
  let root = File_tree.root ftree in
  let tree, stanzas, packages = walk root [] [] in
  let packages =
    String_map.of_alist_multi packages
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
  { file_tree = ftree
  ; tree
  ; stanzas
  ; packages
  }
