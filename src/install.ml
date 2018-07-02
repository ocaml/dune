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

  let compare : t -> t -> Ordering.t = compare

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

  let of_string = function
    | "lib"        -> Some Lib
    | "libexec"    -> Some Libexec
    | "bin"        -> Some Bin
    | "sbin"       -> Some Sbin
    | "toplevel"   -> Some Toplevel
    | "share"      -> Some Share
    | "share_root" -> Some Share_root
    | "etc"        -> Some Etc
    | "doc"        -> Some Doc
    | "stublibs"   -> Some Stublibs
    | "man"        -> Some Man
    | "misc"       -> Some Misc
    | _            -> None

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

  module Paths = struct
    let lib         = Path.in_source "lib"
    let libexec     = Path.in_source "lib"
    let bin         = Path.in_source "bin"
    let sbin        = Path.in_source "sbin"
    let toplevel    = Path.in_source "lib/toplevel"
    let share       = Path.in_source "share"
    let share_root  = Path.in_source "share_root"
    let etc         = Path.in_source "etc"
    let doc         = Path.in_source "doc"
    let stublibs    = Path.in_source "lib/stublibs"
    let man         = Path.in_source "man"
  end

  let install_dir t ~(package : Package.Name.t) =
    let package = Package.Name.to_string package in
    match t with
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
    | Misc       -> invalid_arg "Install.Section.install_dir"
end

module Entry = struct
  type t =
    { src     : Path.t
    ; dst     : string option
    ; section : Section.t
    }

  let make section ?dst src =
    let dst =
      if Sys.win32 then
        let src_base = Path.basename src in
        let dst' =
          match dst with
          | None -> src_base
          | Some s -> s
        in
        match Filename.extension src_base with
        | ".exe" | ".bc" ->
          if Filename.extension dst' <> ".exe" then
            Some (dst' ^ ".exe")
          else
            dst
        | _ -> dst
      else
        dst
    in
    { src
    ; dst
    ; section
    }

  let set_src t src = { t with src }

  let relative_installed_path t ~package =
    let main_dir = Section.install_dir t.section ~package in
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

  let add_install_prefix t ~package ~prefix =
    let opam_will_install_in_this_dir =
      Section.install_dir t.section ~package
    in
    let i_want_to_install_the_file_as =
      Path.append prefix (relative_installed_path t ~package)
    in
    let dst =
      Path.reach i_want_to_install_the_file_as ~from:opam_will_install_in_this_dir
    in
    { t with dst = Some dst }
end

module SMap = Map.Make(Section)

let files entries =
  List.fold_left entries ~init:Path.Set.empty ~f:(fun acc (entry : Entry.t) ->
    Path.Set.add acc entry.src)

let group entries =
  List.map entries ~f:(fun (entry : Entry.t) -> (entry.section, entry))
  |> SMap.of_list_multi
  |> SMap.to_list

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

let pos_of_opam_value : OpamParserTypes.value -> OpamParserTypes.pos = function
  | Bool         (pos, _)       -> pos
  | Int          (pos, _)       -> pos
  | String       (pos, _)       -> pos
  | Relop        (pos, _, _, _) -> pos
  | Prefix_relop (pos, _, _)    -> pos
  | Logop        (pos, _, _, _) -> pos
  | Pfxop        (pos, _, _)    -> pos
  | Ident        (pos, _)       -> pos
  | List         (pos, _)       -> pos
  | Group        (pos, _)       -> pos
  | Option       (pos, _, _)    -> pos
  | Env_binding  (pos, _, _, _) -> pos

let load_install_file path =
  let open OpamParserTypes in
  let file = Opam_file.load path in
  let fail (fname, line, col) fmt =
    let pos : Lexing.position =
      { pos_fname = fname
      ; pos_lnum = line
      ; pos_bol = 0
      ; pos_cnum = col
      }
    in
    Loc.fail { start =  pos; stop = pos } fmt
  in
  List.concat_map file.file_contents ~f:(function
    | Variable (pos, section, files) -> begin
        match Section.of_string section with
        | None -> fail pos "Unknown install section"
        | Some section -> begin
            match files with
            | List (_, l) ->
              List.map l ~f:(function
                | String (_, src) ->
                  { Entry.
                    src = Path.of_string src
                  ; dst = None
                  ; section
                  }
                | Option (_, String (_, src),
                          [String (_, dst)]) ->
                  { Entry.
                    src = Path.of_string src
                  ; dst = Some dst
                  ; section
                  }
                | v ->
                  fail (pos_of_opam_value v)
                    "Invalid value in .install file")
            | v ->
              fail (pos_of_opam_value v)
                "Invalid value for install section"
          end
      end
    | Section (pos, _) ->
      fail pos "Sections are not allowed in .install file")
