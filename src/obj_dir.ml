open! Stdune
open Import

type t =
  { dir: Path.t
  ; obj_dir: Path.t
  ; native_dir: Path.t
  ; byte_dir: Path.t
  ; public_cmi_dir: Path.t option
  }

let has_public_cmi_dir t = Option.is_some t.public_cmi_dir

let public_cmi_dir t =
  Option.value ~default:t.byte_dir t.public_cmi_dir

let dir t = t.dir
let obj_dir t = t.obj_dir
let byte_dir t = t.byte_dir
let all_cmis_dir = byte_dir
let native_dir t = t.native_dir

let all_obj_dirs t ~(mode : Mode.t) =
  let dirs = [t.byte_dir; public_cmi_dir t] in
  let dirs =
    match mode with
    | Byte -> dirs
    | Native -> t.native_dir :: dirs
  in
  Path.Set.of_list dirs
  |> Path.Set.to_list

let make ~dir ~obj_dir ~native_dir ~byte_dir ~public_cmi_dir =
  { dir
  ; obj_dir
  ; native_dir
  ; byte_dir
  ; public_cmi_dir
  }

let make_local ~dir ~has_private_modules lib_name =
  let obj_dir = Utils.library_object_directory ~dir lib_name in
  let public_cmi_dir =
    Option.some_if
      has_private_modules
      (Utils.library_public_cmi_dir ~obj_dir)
  in
  make ~dir
    ~obj_dir
    ~native_dir:(Utils.library_native_dir ~obj_dir)
    ~byte_dir:(Utils.library_byte_dir ~obj_dir)
    ~public_cmi_dir

let make_exe ~dir ~name =
  let obj_dir = Utils.executable_object_directory ~dir name in
  make ~dir
    ~obj_dir
    ~native_dir:(Utils.library_native_dir ~obj_dir)
    ~byte_dir:(Utils.library_byte_dir ~obj_dir)
    ~public_cmi_dir:None

let make_external ~dir =
  { dir
  ; obj_dir = dir
  ; native_dir = dir
  ; byte_dir   = dir
  ; public_cmi_dir = None
  }

let to_sexp { dir; obj_dir; native_dir; byte_dir; public_cmi_dir } =
  let open Sexp.Encoder in
  record
    [ "dir", Path.to_sexp dir
    ; "obj_dir", Path.to_sexp obj_dir
    ; "native_dir", Path.to_sexp native_dir
    ; "byte_dir", Path.to_sexp byte_dir
    ; "public_cmi_dir", (option Path.to_sexp) public_cmi_dir
    ]

let pp fmt { dir; obj_dir; native_dir; byte_dir; public_cmi_dir } =
  Fmt.record fmt
    [ "dir", Fmt.const Path.pp dir
    ; "obj_dir", Fmt.const Path.pp obj_dir
    ; "native_dir", Fmt.const Path.pp native_dir
    ; "byte_dir", Fmt.const Path.pp byte_dir
    ; "public_cmi_dir", Fmt.const (Fmt.optional Path.pp) public_cmi_dir
    ]


let encode
      { dir
      ; obj_dir
      ; native_dir
      ; byte_dir
      ; public_cmi_dir
      } =
  let open Dune_lang.Encoder in
  let extract d =
    Path.descendant ~of_:dir d
    |> Option.value_exn
    |> Path.to_string
  in
  let obj_dir = extract obj_dir in
  let native_dir = extract native_dir in
  let byte_dir = extract byte_dir in
  let public_cmi_dir = Option.map ~f:extract public_cmi_dir in
  record_fields
    [ field "obj_dir" ~equal:String.equal ~default:"." string obj_dir
    ; field "native_dir" ~equal:String.equal ~default:"." string native_dir
    ; field "byte_dir" ~equal:String.equal ~default:"." string byte_dir
    ; field_o "public_cmi_dir" string public_cmi_dir
    ]


let decode ~dir =
  let open Dune_lang.Decoder in
  fields (
    let%map obj_dir = field ~default:"." "obj_dir" string
    and native_dir = field ~default:"." "native_dir" string
    and byte_dir = field ~default:"." "byte_dir" string
    and public_cmi_dir = field_o "public_cmi_dir" string
    in
    make ~dir
      ~obj_dir:(Path.relative dir obj_dir)
      ~native_dir:(Path.relative dir native_dir)
      ~byte_dir:(Path.relative dir byte_dir)
      ~public_cmi_dir:(Option.map ~f:(Path.relative dir) public_cmi_dir)
  )
