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

let to_string = function
  | Lib -> "lib"
  | Lib_root -> "lib_root"
  | Libexec -> "libexec"
  | Libexec_root -> "libexec_root"
  | Bin -> "bin"
  | Sbin -> "sbin"
  | Toplevel -> "toplevel"
  | Share -> "share"
  | Share_root -> "share_root"
  | Etc -> "etc"
  | Doc -> "doc"
  | Stublibs -> "stublibs"
  | Man -> "man"
  | Misc -> "misc"

let of_string = function
  | "lib" -> Some Lib
  | "lib_root" -> Some Lib_root
  | "libexec" -> Some Libexec
  | "libexec_root" -> Some Libexec_root
  | "bin" -> Some Bin
  | "sbin" -> Some Sbin
  | "toplevel" -> Some Toplevel
  | "share" -> Some Share
  | "share_root" -> Some Share_root
  | "etc" -> Some Etc
  | "doc" -> Some Doc
  | "stublibs" -> Some Stublibs
  | "man" -> Some Man
  | "misc" -> Some Misc
  | _ -> None
