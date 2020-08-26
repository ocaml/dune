open! Dune_engine
open! Stdune
open Import

module Section0 = struct
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

  let compare : t -> t -> Ordering.t = Poly.compare

  let to_dyn _ = Dyn.opaque
end

(* The path after the man section mangling done by opam-installer. This roughly
   follows [add_man_section_dir] in [src/format/opamFile.ml] in opam. *)
module Dst : sig
  type t

  val to_string : t -> string

  val to_install_file :
    t -> src_basename:string -> section:Section0.t -> string option

  val of_install_file :
    string option -> src_basename:string -> section:Section0.t -> t

  val explicit : string -> t

  val compare : t -> t -> Ordering.t

  val infer : src_basename:string -> Section0.t -> t
end = struct
  type t = string

  let to_string t = t

  let explicit t = t

  let compare = Poly.compare

  let man_subdir s =
    let s =
      match String.drop_suffix ~suffix:".gz" s with
      | Some s -> s
      | None -> s
    in
    match String.rsplit2 ~on:'.' s with
    | None -> None
    | Some (_, "") -> None
    | Some (_, r) -> (
      match r.[0] with
      | '1' .. '8' as c -> Some (sprintf "man%c" c)
      | _ -> None )

  let infer ~src_basename:p section =
    match section with
    | Section0.Man -> (
      match man_subdir p with
      | Some subdir -> Filename.concat subdir p
      | None -> p )
    | _ -> p

  let of_install_file t ~src_basename ~section =
    match t with
    | None -> infer ~src_basename section
    | Some s -> s

  let to_install_file t ~src_basename ~section =
    match t with
    | s ->
      let s' = infer ~src_basename section in
      if String.equal s s' then
        None
      else
        Some s
end

module Section = struct
  include Section0
  include Comparable.Make (Section0)

  let all =
    Set.of_list
      [ Lib
      ; Lib_root
      ; Libexec
      ; Libexec_root
      ; Bin
      ; Sbin
      ; Toplevel
      ; Share
      ; Share_root
      ; Etc
      ; Doc
      ; Stublibs
      ; Man
      ; Misc
      ]

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

  let parse_string s =
    match of_string s with
    | Some s -> Ok s
    | None -> Error (sprintf "invalid section: %s" s)

  let decode =
    let open Dune_lang.Decoder in
    enum
      [ ("lib", Lib)
      ; ("lib_root", Lib_root)
      ; ("libexec", Libexec)
      ; ("libexec_root", Libexec_root)
      ; ("bin", Bin)
      ; ("sbin", Sbin)
      ; ("toplevel", Toplevel)
      ; ("share", Share)
      ; ("share_root", Share_root)
      ; ("etc", Etc)
      ; ("doc", Doc)
      ; ("stublibs", Stublibs)
      ; ("man", Man)
      ; ("misc", Misc)
      ]

  let should_set_executable_bit = function
    | Lib
    | Lib_root
    | Toplevel
    | Share
    | Share_root
    | Etc
    | Doc
    | Man
    | Misc ->
      false
    | Libexec
    | Libexec_root
    | Bin
    | Sbin
    | Stublibs ->
      true

  module Paths = struct
    type t =
      { lib : Path.t
      ; lib_root : Path.t
      ; libexec : Path.t
      ; libexec_root : Path.t
      ; bin : Path.t
      ; sbin : Path.t
      ; toplevel : Path.t
      ; share : Path.t
      ; share_root : Path.t
      ; etc : Path.t
      ; doc : Path.t
      ; stublibs : Path.t
      ; man : Path.t
      }

    let make ~package ~destdir ?(libdir = Path.relative destdir "lib")
        ?(mandir = Path.relative destdir "man") () =
      let package = Package.Name.to_string package in
      let lib_root = libdir in
      let libexec_root = libdir in
      let share_root = Path.relative destdir "share" in
      let etc_root = Path.relative destdir "etc" in
      let doc_root = Path.relative destdir "doc" in
      { lib_root
      ; libexec_root
      ; share_root
      ; bin = Path.relative destdir "bin"
      ; sbin = Path.relative destdir "sbin"
      ; man = mandir
      ; toplevel = Path.relative libdir "toplevel"
      ; stublibs = Path.relative libdir "stublibs"
      ; lib = Path.relative lib_root package
      ; libexec = Path.relative libexec_root package
      ; share = Path.relative share_root package
      ; etc = Path.relative etc_root package
      ; doc = Path.relative doc_root package
      }

    let get t section =
      match section with
      | Lib -> t.lib
      | Lib_root -> t.lib_root
      | Libexec -> t.libexec
      | Libexec_root -> t.libexec_root
      | Bin -> t.bin
      | Sbin -> t.sbin
      | Toplevel -> t.toplevel
      | Share -> t.share
      | Share_root -> t.share_root
      | Etc -> t.etc
      | Doc -> t.doc
      | Stublibs -> t.stublibs
      | Man -> t.man
      | Misc -> Code_error.raise "Install.Paths.get" []

    let install_path t section p =
      Path.relative (get t section) (Dst.to_string p)
  end
end

module Entry = struct
  type 'src t =
    { src : 'src
    ; dst : Dst.t
    ; section : Section.t
    }

  let compare x y =
    let c = Path.Build.compare x.src y.src in
    if c <> Eq then
      c
    else
      let c = Dst.compare x.dst y.dst in
      if c <> Eq then
        c
      else
        Section.compare x.section y.section

  let adjust_dst ~src ~dst ~section =
    let error var =
      User_error.raise
        ~loc:(String_with_vars.Var.loc var)
        [ Pp.textf
            "Because this file is installed in the 'bin' section, you cannot \
             use the variable %s in its basename."
            (String_with_vars.Var.describe var)
        ]
    in
    let is_source_executable () =
      let has_ext ext =
        match String_with_vars.Partial.is_suffix ~suffix:ext src with
        | Unknown var -> error var
        | Yes -> true
        | No -> false
      in
      has_ext ".exe" || has_ext ".bc"
    in
    let src_basename () =
      match src with
      | Expanded s -> Filename.basename s
      | Unexpanded src -> (
        match String_with_vars.known_suffix src with
        | Full s -> Filename.basename s
        | Partial (var, suffix) -> (
          match String.rsplit2 ~on:'/' suffix with
          | Some (_, basename) -> basename
          | None -> error var ) )
    in
    match dst with
    | Some dst' when Filename.extension dst' = ".exe" -> Dst.explicit dst'
    | _ ->
      let dst =
        match dst with
        | None -> Dst.infer ~src_basename:(src_basename ()) section
        | Some dst -> Dst.explicit dst
      in
      let is_executable = is_source_executable () in
      if
        Sys.win32 && is_executable
        && Filename.extension (Dst.to_string dst) <> ".exe"
      then
        Dst.explicit (Dst.to_string dst ^ ".exe")
      else
        dst

  let make section ?dst src =
    let dst =
      adjust_dst ~src:(Expanded (Path.to_string (Path.build src))) ~dst ~section
    in
    { src; dst; section }

  let set_src t src = { t with src }

  let relative_installed_path t ~paths =
    Section.Paths.install_path paths t.section t.dst

  let add_install_prefix t ~paths ~prefix =
    let opam_will_install_in_this_dir = Section.Paths.get paths t.section in
    let i_want_to_install_the_file_as =
      relative_installed_path t ~paths
      |> Path.as_in_source_tree_exn |> Path.append_source prefix
    in
    let dst =
      Path.reach i_want_to_install_the_file_as
        ~from:opam_will_install_in_this_dir
    in
    { t with dst = Dst.explicit dst }

  let of_install_file ~src ~dst ~section =
    { src
    ; section
    ; dst = Dst.of_install_file ~section ~src_basename:(Path.basename src) dst
    }
end

let files entries =
  Path.Set.of_list_map entries ~f:(fun (entry : Path.Build.t Entry.t) ->
      Path.build entry.src)

let group entries =
  List.map entries ~f:(fun (entry : Path.Build.t Entry.t) ->
      (entry.section, entry))
  |> Section.Map.of_list_multi

let gen_install_file entries =
  let buf = Buffer.create 4096 in
  let pr fmt = Printf.bprintf buf (fmt ^^ "\n") in
  Section.Map.iteri (group entries) ~f:(fun section entries ->
      pr "%s: [" (Section.to_string section);
      List.sort ~compare:Entry.compare entries
      |> List.iter ~f:(fun (e : Path.Build.t Entry.t) ->
             let src = Path.to_string (Path.build e.src) in
             match
               Dst.to_install_file
                 ~src_basename:(Path.Build.basename e.src)
                 ~section:e.section e.dst
             with
             | None -> pr "  %S" src
             | Some dst -> pr "  %S {%S}" src dst);
      pr "]");
  Buffer.contents buf

let pos_of_opam_value : OpamParserTypes.value -> OpamParserTypes.pos = function
  | Bool (pos, _) -> pos
  | Int (pos, _) -> pos
  | String (pos, _) -> pos
  | Relop (pos, _, _, _) -> pos
  | Prefix_relop (pos, _, _) -> pos
  | Logop (pos, _, _, _) -> pos
  | Pfxop (pos, _, _) -> pos
  | Ident (pos, _) -> pos
  | List (pos, _) -> pos
  | Group (pos, _) -> pos
  | Option (pos, _, _) -> pos
  | Env_binding (pos, _, _, _) -> pos

let load_install_file path =
  let open OpamParserTypes in
  let file = Opam_file.load path in
  let fail (fname, line, col) msg =
    let pos : Lexing.position =
      { pos_fname = fname; pos_lnum = line; pos_bol = 0; pos_cnum = col }
    in
    User_error.raise ~loc:{ start = pos; stop = pos } [ Pp.text msg ]
  in
  List.concat_map file.file_contents ~f:(function
    | Variable (pos, section, files) -> (
      match Section.of_string section with
      | None -> fail pos "Unknown install section"
      | Some section -> (
        match files with
        | List (_, l) ->
          let install_file src dst =
            let src = Path.of_string src in
            Entry.of_install_file ~src ~dst ~section
          in
          List.map l ~f:(function
            | String (_, src) -> install_file src None
            | Option (_, String (_, src), [ String (_, dst) ]) ->
              install_file src (Some dst)
            | v -> fail (pos_of_opam_value v) "Invalid value in .install file")
        | v -> fail (pos_of_opam_value v) "Invalid value for install section" )
      )
    | Section (pos, _) -> fail pos "Sections are not allowed in .install file")
