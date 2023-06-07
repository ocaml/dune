open Import

type 'a t =
  { lib_root : 'a
  ; libexec_root : 'a
  ; bin : 'a
  ; sbin : 'a
  ; share_root : 'a
  ; etc_root : 'a
  ; doc_root : 'a
  ; man : 'a
  }

let make prefix ~relative =
  let lib_root = relative prefix "lib" in
  { lib_root
  ; libexec_root = lib_root
  ; bin = relative prefix "bin"
  ; sbin = relative prefix "sbin"
  ; share_root = relative prefix "share"
  ; man = relative prefix "man"
  ; doc_root = relative prefix "doc"
  ; etc_root = relative prefix "etc"
  }

let opam_from_prefix prefix = make prefix ~relative:Path.relative

let complete x =
  match x.libexec_root with
  | Some _ -> x
  | None -> { x with libexec_root = x.lib_root }

let map ~f x =
  { lib_root = f x.lib_root
  ; libexec_root = f x.libexec_root
  ; bin = f x.bin
  ; sbin = f x.sbin
  ; share_root = f x.share_root
  ; etc_root = f x.etc_root
  ; doc_root = f x.doc_root
  ; man = f x.man
  }

let map2 ~f x y =
  { lib_root = f x.lib_root y.lib_root
  ; libexec_root = f x.libexec_root y.libexec_root
  ; bin = f x.bin y.bin
  ; sbin = f x.sbin y.sbin
  ; share_root = f x.share_root y.share_root
  ; etc_root = f x.etc_root y.etc_root
  ; doc_root = f x.doc_root y.doc_root
  ; man = f x.man y.man
  }

let first_has_priority x y =
  map2 x y ~f:(fun x y ->
      match x with
      | Some _ -> x
      | None -> y)

let ocamlpath_sep = if Sys.cygwin then ';' else Bin.path_sep

let to_env_without_path t =
  [ ("CAML_LD_LIBRARY_PATH", Path.Build.relative t.lib_root "stublibs")
  ; ("OCAMLPATH", t.lib_root)
  ; ("OCAMLTOP_INCLUDE_PATH", Path.Build.relative t.lib_root "toplevel")
  ; ("OCAMLFIND_IGNORE_DUPS_IN", t.lib_root)
  ; ("MANPATH", t.man)
  ]

let sep = function
  | "OCAMLFIND_IGNORE_DUPS_IN" | "OCAMLPATH" -> Some ocamlpath_sep
  | _ -> None

let add_to_env t env =
  to_env_without_path t
  |> List.fold_left ~init:env ~f:(fun env (var, path) ->
         Env.update env ~var ~f:(fun _PATH ->
             let path_sep = sep var in
             Some (Bin.cons_path ?path_sep (Path.build path) ~_PATH)))
