open! Stdune
open Import

module External = struct
  type t =
    { public_dir : Path.t
    ; private_dir : Path.t option
    }

  let has_public_cmi_dir t = Option.is_some t.private_dir

  let public_cmi_dir t = t.public_dir

  let to_dyn { public_dir; private_dir } =
    let open Dyn.Encoder in
    record
      [ "public_dir", Path.to_dyn public_dir
      ; "private_dir", option Path.to_dyn private_dir
      ]

  let encode
        { public_dir
        ; private_dir
        } =
    let open Dune_lang.Encoder in
    let extract d =
      Path.descendant ~of_:public_dir d
      |> Option.value_exn
      |> Path.to_string
    in
    let private_dir = Option.map ~f:extract private_dir in
    record_fields
      [ field_o "private_dir" string private_dir
      ]

  let decode ~dir =
    let open Dune_lang.Decoder in
    fields (
      let+ private_dir = field_o "private_dir" string
      in
      let public_dir = dir in
      let private_dir = Option.map ~f:(Path.relative dir) private_dir in
      { public_dir
      ; private_dir
      }
    )

  let byte_dir t = t.public_dir
  let native_dir t = t.public_dir
  let dir t = t.public_dir
  let obj_dir t = t.public_dir

  let all_obj_dirs t ~mode:_ =
    [t.public_dir]
end

module Local = struct
  type t =
    { dir: Path.t
    ; obj_dir: Path.t
    ; native_dir: Path.t
    ; byte_dir: Path.t
    ; public_cmi_dir: Path.t option
    }

  let to_dyn { dir; obj_dir; native_dir; byte_dir; public_cmi_dir } =
    let open Dyn.Encoder in
    record
      [ "dir", Path.to_dyn dir
      ; "obj_dir", Path.to_dyn obj_dir
      ; "native_dir", Path.to_dyn native_dir
      ; "byte_dir", Path.to_dyn byte_dir
      ; "public_cmi_dir", option Path.to_dyn public_cmi_dir
      ]

  let make ~dir ~obj_dir ~native_dir ~byte_dir ~public_cmi_dir =
    { dir
    ; obj_dir
    ; native_dir
    ; byte_dir
    ; public_cmi_dir
    }

  let has_public_cmi_dir t = Option.is_some t.public_cmi_dir

  let public_cmi_dir t =
    Option.value ~default:t.byte_dir t.public_cmi_dir

  let dir t = t.dir
  let obj_dir t = t.obj_dir
  let byte_dir t = t.byte_dir
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

  let make_lib ~dir ~has_private_modules lib_name =
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
end

type t =
  | External of External.t
  | Local of Local.t

let has_public_cmi_dir = function
  | External e -> External.has_public_cmi_dir e
  | Local e -> Local.has_public_cmi_dir e

let encode = function
  | Local obj_dir ->
    Exn.code_error "Obj_dir.encode: local paths cannot be encoded"
      [ "obj_dir", Dyn.to_sexp (Local.to_dyn obj_dir)
      ]
  | External e -> External.encode e

let decode ~dir =
  let open Dune_lang.Decoder in
  let+ external_ = External.decode ~dir in
  External external_

let make_exe ~dir ~name = Local (Local.make_exe ~dir ~name)
let make_lib ~dir ~has_private_modules lib_name =
  Local (Local.make_lib ~dir ~has_private_modules lib_name)

let make_external ~dir =
  External { External.public_dir = dir; private_dir = None }

let all_obj_dirs t ~mode =
  match t with
  | External e -> External.all_obj_dirs e ~mode
  | Local e -> Local.all_obj_dirs e ~mode

let public_cmi_dir = function
  | External e -> External.public_cmi_dir e
  | Local e -> Local.public_cmi_dir e

let byte_dir = function
  | External e -> External.byte_dir e
  | Local e -> Local.byte_dir e

let native_dir = function
  | External e -> External.native_dir e
  | Local e -> Local.native_dir e

let dir = function
  | External e -> External.dir e
  | Local e -> Local.dir e

let obj_dir = function
  | External e -> External.obj_dir e
  | Local e -> Local.obj_dir e

let to_dyn =
  let open Dyn.Encoder in
  function
  | Local e -> constr "Local" [Local.to_dyn e]
  | External e -> constr "External" [External.to_dyn e]

let to_sexp t = Dyn.to_sexp (to_dyn t)
let pp fmt t = Dyn.pp fmt (to_dyn t)
