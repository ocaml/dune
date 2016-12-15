open Import

module Section = struct
  type t =
    | Lib
    | Libexec
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

  let compare : t -> t -> int = compare

  let to_string = function
    | Lib        -> "lib"
    | Libexec    -> "libexec"
    | Bin        -> "bin"
    | Sbin       -> "sbin"
    | Toplevel   -> "toplevel"
    | Share      -> "share"
    | Share_root -> "share_root"
    | Etc        -> "etc"
    | Doc        -> "doc"
    | Stublibs   -> "stublibs"
    | Man        -> "man"
    | Misc       -> "misc"

  let t =
    let open Sexp.Of_sexp in
    sum
      [ cstr "lib"        [] Lib
      ; cstr "libexec"    [] Libexec
      ; cstr "bin"        [] Bin
      ; cstr "sbin"       [] Sbin
      ; cstr "toplevel"   [] Toplevel
      ; cstr "share"      [] Share
      ; cstr "share_root" [] Share_root
      ; cstr "etc"        [] Etc
      ; cstr "doc"        [] Doc
      ; cstr "stublibs"   [] Stublibs
      ; cstr "man"        [] Man
      ; cstr "misc"       [] Misc
      ]
end

module Entry = struct
  type t =
    { src     : Path.t
    ; dst     : string option
    ; section : Section.t
    }

  let make section ?dst src =
    { src
    ; dst
    ; section
    }
end

module SMap = Map.Make(Section)

let files entries =
  List.fold_left entries ~init:Path.Set.empty ~f:(fun acc (entry : Entry.t) ->
    Path.Set.add entry.src acc)

let group entries =
  List.map entries ~f:(fun (entry : Entry.t) -> (entry.section, entry))
  |> SMap.of_alist_multi
  |> SMap.bindings

let write_install_file file entries =
  with_file_out (Path.to_string file) ~f:(fun oc ->
    let pr fmt = Printf.fprintf oc (fmt ^^ "\n") in
    List.iter (group entries) ~f:(fun (section, entries) ->
      pr "%s: [" (Section.to_string section);
      List.iter entries ~f:(fun (e : Entry.t) ->
        let src = Path.to_string e.src in
        match e.dst with
        | None     -> pr "  %S"      src
        | Some dst -> pr "  %S {%S}" src dst);
      pr "]"))
