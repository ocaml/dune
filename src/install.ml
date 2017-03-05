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
    enum
      [ "lib"        , Lib
      ; "libexec"    , Libexec
      ; "bin"        , Bin
      ; "sbin"       , Sbin
      ; "toplevel"   , Toplevel
      ; "share"      , Share
      ; "share_root" , Share_root
      ; "etc"        , Etc
      ; "doc"        , Doc
      ; "stublibs"   , Stublibs
      ; "man"        , Man
      ; "misc"       , Misc
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

  module Paths = struct
    let lib         = Path.(relative root) "lib"
    let libexec     = Path.(relative root) "lib"
    let bin         = Path.(relative root) "bin"
    let sbin        = Path.(relative root) "sbin"
    let toplevel    = Path.(relative root) "lib/toplevel"
    let share       = Path.(relative root) "share"
    let share_root  = Path.(relative root) "share_root"
    let etc         = Path.(relative root) "etc"
    let doc         = Path.(relative root) "doc"
    let stublibs    = Path.(relative root) "lib/stublibs"
    let man         = Path.(relative root) "man"
  end

  let relative_installed_path t ~package =
    let main_dir =
      match t.section with
      | Bin        -> Paths.bin
      | Sbin       -> Paths.sbin
      | Toplevel   -> Paths.toplevel
      | Share_root -> Paths.share_root
      | Stublibs   -> Paths.stublibs
      | Man        -> Paths.man
      | Lib        -> Path.relative Paths.lib     package
      | Libexec    -> Path.relative Paths.libexec package
      | Share      -> Path.relative Paths.share   package
      | Etc        -> Path.relative Paths.etc     package
      | Doc        -> Path.relative Paths.doc     package
      | Misc       -> invalid_arg "Install.Entry.relative_installed_path"
    in
    let dst =
      match t.dst with
      | Some x -> x
      | None ->
        let dst = Path.basename t.src in
        match t.section with
        | Man -> begin
            match String.rsplit2 dst ~on:'.' with
            | None -> dst
            | Some (_, sec) -> sprintf "man%s/%s" sec dst
          end
        | _ -> dst
    in
    Path.relative main_dir dst
end

module SMap = Map.Make(Section)

let files entries =
  List.fold_left entries ~init:Path.Set.empty ~f:(fun acc (entry : Entry.t) ->
    Path.Set.add entry.src acc)

let group entries =
  List.map entries ~f:(fun (entry : Entry.t) -> (entry.section, entry))
  |> SMap.of_alist_multi
  |> SMap.bindings

let gen_install_file entries =
  let buf = Buffer.create 4096 in
  let pr fmt = Printf.bprintf buf (fmt ^^ "\n") in
  List.iter (group entries) ~f:(fun (section, entries) ->
    pr "%s: [" (Section.to_string section);
      List.iter entries ~f:(fun (e : Entry.t) ->
        let src = Path.to_string e.src in
        match e.dst with
        | None     -> pr "  %S"      src
        | Some dst -> pr "  %S {%S}" src dst);
    pr "]");
  Buffer.contents buf
