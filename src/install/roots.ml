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

let opam_from_prefix prefix =
  let lib_root = Path.relative prefix "lib" in
  { lib_root
  ; libexec_root = lib_root
  ; bin = Path.relative prefix "bin"
  ; sbin = Path.relative prefix "sbin"
  ; share_root = Path.relative prefix "share"
  ; man = Path.relative prefix "man"
  ; doc_root = Path.relative prefix "doc"
  ; etc_root = Path.relative prefix "etc"
  }

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
