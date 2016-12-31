open Import
open Jbuild_types

type conf =
  { file_tree : File_tree.t
  ; tree      : Alias.tree
  ; stanzas   : (Path.t * Jbuild_types.Stanza.t list) list
  ; packages  : string list
  }

let load fn ~dir = (dir, Sexp_load.many fn Stanza.t)

let load ftree =
  let rec walk dir stanzas =
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
    let children, stanzas =
      String_map.fold sub_dirs ~init:([], stanzas) ~f:(fun ~key:_ ~data:dir (children, stanzas) ->
          let child, stanzas = walk dir stanzas in
          (child :: children, stanzas))
    in
    let stanzas =
      if String_set.mem "jbuild" files then
        load (Path.to_string (Path.relative path "jbuild")) ~dir:path :: stanzas
      else
        stanzas
    in
    (Alias.Node (path, children), stanzas)
  in
  let ftree = File_tree.load Path.root in
  let root = File_tree.root ftree in
  let tree, stanzas = walk root [] in
  let packages =
    String_set.fold (File_tree.Dir.files root) ~init:[] ~f:(fun fn acc ->
      match Filename.split_ext fn with
      | Some (pkg, ".opam") -> pkg :: acc
      | _ -> acc)
  in
  { file_tree = ftree
  ; tree
  ; stanzas
  ; packages
  }
