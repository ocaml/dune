open! Stdune
open Import
module Entry = Install.Entry
module Dst = Entry.Dst

let make_with_site (section : Section_with_site.t) ?dst get_section ~kind src =
  match section with
  | Section section -> Memo.return (Entry.make section ?dst ~kind src)
  | Site { pkg; site; loc } ->
    let open Memo.O in
    let+ section = get_section ~loc ~pkg ~site in
    let dst =
      let dst = Entry.adjust_dst' ~src ~dst ~section in
      Dst.add_prefix (Site.to_string site) dst
    in
    let dst_with_pkg_prefix = Dst.add_prefix (Package.Name.to_string pkg) dst in
    let (section : Section.t), dst =
      match section with
      | Lib -> Lib_root, dst_with_pkg_prefix
      | Libexec -> Libexec_root, dst_with_pkg_prefix
      | Share -> Share_root, dst_with_pkg_prefix
      | Etc | Doc ->
        User_error.raise [ Pp.textf "Can't have site in etc and doc for opam" ]
      | Lib_root
      | Libexec_root
      | Bin
      | Sbin
      | Toplevel
      | Share_root
      | Stublibs
      | Man
      | Misc -> section, dst
    in
    Entry.make_with_dst section dst ~kind ~src
;;

module Entry_with_site = struct
  type 'src t =
    { src : 'src
    ; dst : Dst.t
    ; section : Section_with_site.t
    }
end
