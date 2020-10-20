type t =
  | Lib
  | Lib_root
  | Libexec
  | Libexec_root
  | Bin
  | Sbin
  | Toplevel
  | Share
  | Share_root
  | Etc
  | Doc
  | Stublibs
  | Man
  | Misc

let all =
  [ (Lib, "lib")
  ; (Lib_root, "lib_root")
  ; (Libexec, "libexec")
  ; (Libexec_root, "libexec_root")
  ; (Bin, "bin")
  ; (Sbin, "sbin")
  ; (Toplevel, "toplevel")
  ; (Share, "share")
  ; (Share_root, "share_root")
  ; (Etc, "etc")
  ; (Doc, "doc")
  ; (Stublibs, "stublibs")
  ; (Man, "man")
  ; (Misc, "misc")
  ]

let to_string t = List.assoc t all

let of_string x =
  List.find_map
    (fun (t, s) ->
      if s = x then
        Some t
      else
        None)
    all
