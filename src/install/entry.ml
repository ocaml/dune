open! Stdune
open Import

(* The path after the man section mangling done by opam-installer. This roughly
   follows [add_man_section_dir] in [src/format/opamFile.ml] in opam. *)
module Dst : sig
  type t

  val to_string : t -> string
  val concat_all : t -> string list -> t
  val add_prefix : string -> t -> t
  val add_suffix : t -> string -> t
  val to_install_file : t -> src_basename:string -> section:Section.t -> string option
  val of_install_file : string option -> src_basename:string -> section:Section.t -> t
  val explicit : string -> t
  val compare : t -> t -> Ordering.t
  val infer : src_basename:string -> Section.t -> t

  include Dune_lang.Conv.S with type t := t

  val to_dyn : t -> Dyn.t
  val install_path : Path.t Paths.t -> Section.t -> t -> Path.t
end = struct
  type t = string

  let to_string t = t
  let concat_all t suffixes = List.fold_left suffixes ~init:t ~f:Filename.concat
  let add_prefix p t = Filename.concat p t
  let add_suffix t p = Filename.concat t p
  let explicit t = t
  let compare = String.compare

  let man_subdir s =
    let s =
      match String.drop_suffix ~suffix:".gz" s with
      | Some s -> s
      | None -> s
    in
    match String.rsplit2 ~on:'.' s with
    | None -> None
    | Some (_, "") -> None
    | Some (_, r) ->
      (match r.[0] with
       | '1' .. '8' as c -> Some (sprintf "man%c" c)
       | _ -> None)
  ;;

  let infer ~src_basename:p section =
    match section with
    | Section.Man ->
      (match man_subdir p with
       | Some subdir -> Filename.concat subdir p
       | None -> p)
    | _ -> p
  ;;

  let of_install_file t ~src_basename ~section =
    match t with
    | None -> infer ~src_basename section
    | Some s -> s
  ;;

  let to_install_file t ~src_basename ~section =
    match t with
    | s ->
      let s' = infer ~src_basename section in
      if String.equal s s' then None else Some s
  ;;

  let decode = Dune_sexp.Decoder.string
  let encode = Dune_sexp.Encoder.string
  let to_dyn = Dyn.string
  let install_path t section p = Path.relative (Paths.get t section) (to_string p)
end

type kind =
  [ `File
  | `Directory
  | `Source_tree
  ]

type 'src t =
  { src : 'src
  ; kind : kind
  ; dst : Dst.t
  ; section : Section.t
  ; optional : bool
  }

let map_dst t ~f = { t with dst = f t.dst }

let to_dyn { src; kind; dst; section; optional } =
  let open Dyn in
  let dyn_of_kind = function
    | `File -> String "file"
    | `Directory -> String "directory"
    | `Source_tree -> String "source_tree"
  in
  record
    [ "src", Path.Build.to_dyn src
    ; "kind", dyn_of_kind kind
    ; "dst", Dst.to_dyn dst
    ; "section", Section.to_dyn section
    ; "optional", Dyn.Bool optional
    ]
;;

module Sourced = struct
  type source =
    | User of Loc.t
    | Dune

  type nonrec t =
    { source : source
    ; entry : Path.Build.t t
    }

  let create ?loc entry =
    { source =
        (match loc with
         | None -> Dune
         | Some loc -> User loc)
    ; entry
    }
  ;;

  let to_dyn { source; entry } =
    let open Dyn in
    let source_to_dyn = function
      | Dune -> Variant ("Dune", [])
      | User loc -> Variant ("User", [ Loc.to_dyn loc ])
    in
    Record [ "source", source_to_dyn source; "entry", to_dyn entry ]
  ;;
end

let compare compare_src { optional; src; dst; section; kind } t =
  let open Ordering.O in
  let= () = Section.compare section t.section in
  let= () = Dst.compare dst t.dst in
  let= () = compare_src src t.src in
  let= () = Bool.compare optional t.optional in
  Poly.compare kind t.kind
;;

let adjust_dst_gen =
  let error (source_pform : Dune_lang.Template.Pform.t) =
    User_error.raise
      ~loc:source_pform.loc
      [ Pp.textf
          "Because this file is installed in the 'bin' section, you cannot use the %s %s \
           in its basename."
          (Dune_lang.Template.Pform.describe_kind source_pform)
          (Dune_lang.Template.Pform.describe source_pform)
      ]
  in
  fun ~(src_suffix : String_with_vars.known_suffix) ~dst ~section ->
    match dst with
    | Some dst' when Filename.extension dst' = ".exe" -> Dst.explicit dst'
    | _ ->
      let dst =
        match dst with
        | None ->
          let src_basename =
            match src_suffix with
            | Full s -> Filename.basename s
            | Partial { source_pform; suffix } ->
              (match String.rsplit2 ~on:'/' suffix with
               | Some (_, basename) -> basename
               | None -> error source_pform)
          in
          Dst.infer ~src_basename section
        | Some dst -> Dst.explicit dst
      in
      let is_executable =
        let has_ext ext =
          match src_suffix with
          | Full s -> String.is_suffix s ~suffix:ext
          | Partial { source_pform; suffix } ->
            if String.is_suffix suffix ~suffix:ext
            then true
            else if String.is_suffix ext ~suffix
            then error source_pform
            else false
        in
        has_ext ".exe" || has_ext ".bc"
      in
      if Sys.win32 && is_executable && Filename.extension (Dst.to_string dst) <> ".exe"
      then Dst.explicit (Dst.to_string dst ^ ".exe")
      else dst
;;

let adjust_dst ~src ~dst ~section =
  adjust_dst_gen ~src_suffix:(String_with_vars.known_suffix src) ~dst ~section
;;

let adjust_dst' ~src ~dst ~section =
  adjust_dst_gen ~src_suffix:(Full (Path.to_string (Path.build src))) ~dst ~section
;;

let make section ?dst ~kind src =
  let dst = adjust_dst' ~src ~dst ~section in
  { optional = false; src; dst; section; kind }
;;

let make_with_dst section dst ~kind ~src = { optional = false; src; dst; section; kind }
let set_src t src = { t with src }
let set_kind t kind = { t with kind }
let relative_installed_path t ~paths = Dst.install_path paths t.section t.dst

let add_install_prefix t ~paths ~prefix =
  let opam_will_install_in_this_dir = Paths.get paths t.section in
  let i_want_to_install_the_file_as =
    relative_installed_path t ~paths
    |> Path.as_in_source_tree_exn
    |> Path.append_source prefix
  in
  let dst =
    Path.reach i_want_to_install_the_file_as ~from:opam_will_install_in_this_dir
  in
  { t with dst = Dst.explicit dst }
;;

let of_install_file ~optional ~src ~dst ~section =
  { src
  ; section
  ; dst = Dst.of_install_file ~section ~src_basename:(Path.basename src) dst
  ; kind = `File
  ; optional
  }
;;

let dyn_of_kind =
  let open Dyn in
  function
  | `File -> variant "File" []
  | `Directory -> variant "Directory" []
  | `Source_tree -> variant "Source_tree" []
;;

let to_dyn f { optional; src; kind; dst; section } =
  let open Dyn in
  record
    [ "src", f src
    ; "kind", dyn_of_kind kind
    ; "dst", Dst.to_dyn dst
    ; "section", Section.to_dyn section
    ; "optional", Dyn.bool optional
    ]
;;

let group entries =
  List.map entries ~f:(fun (entry : _ t) -> entry.section, entry)
  |> Section.Map.of_list_multi
;;

let gen_install_file entries =
  let buf = Buffer.create 4096 in
  let pr fmt = Printf.bprintf buf (fmt ^^ "\n") in
  Section.Map.iteri (group entries) ~f:(fun section entries ->
    pr "%s: [" (Section.to_string section);
    List.sort ~compare:(compare Path.compare) entries
    |> List.iter ~f:(fun (e : Path.t t) ->
      let src = Path.to_string e.src in
      match
        Dst.to_install_file ~src_basename:(Path.basename e.src) ~section:e.section e.dst
      with
      | None -> pr "  %S" src
      | Some dst -> pr "  %S {%S}" src dst);
    pr "]");
  Buffer.contents buf
;;

let load_install_file path local =
  let open OpamParserTypes.FullPos in
  let file = Io.with_lexbuf_from_file path ~f:Dune_pkg.Opam_file.parse in
  let fail { filename = pos_fname; start; stop } msg =
    let position_of_loc (pos_lnum, pos_cnum) =
      { Lexing.pos_fname; pos_lnum; pos_bol = 0; pos_cnum }
    in
    let start = position_of_loc start in
    let stop = position_of_loc stop in
    let loc = Loc.create ~start ~stop in
    User_error.raise ~loc [ Pp.text msg ]
  in
  List.concat_map file.file_contents ~f:(function
    | { pelem = Variable (section, files); pos } ->
      (match Section.of_string section.pelem with
       | None -> fail pos "Unknown install section"
       | Some section ->
         (match files with
          | { pelem = List l; _ } ->
            let install_file src dst =
              let optional, src =
                match String.drop_prefix src ~prefix:"?" with
                | None -> false, src
                | Some src -> true, src
              in
              let src =
                if Filename.is_relative src
                then local (Path.Local.of_string src)
                else Path.external_ (Path.External.of_string src)
              in
              of_install_file ~optional ~src ~dst ~section
            in
            List.map l.pelem ~f:(function
              | { pelem = String src; _ } -> install_file src None
              | { pelem =
                    Option
                      ( { pelem = String src; _ }
                      , { pelem = [ { pelem = String dst; _ } ]; _ } )
                ; _
                } -> install_file src (Some dst)
              | { pelem = _; pos } -> fail pos "Invalid value in .install file")
          | { pelem = _; pos } -> fail pos "Invalid value for install section"))
    | { pelem = Section _; pos } -> fail pos "Sections are not allowed in .install file")
;;
