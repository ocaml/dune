open Import

module Name = struct
  type t = string

  let t = Sexp.atom

  let add_suffix = (^)

  let compare = compare
  let of_string = String.capitalize
  let to_string x = x

  let uncapitalize = String.uncapitalize

  let pp = Format.pp_print_string
  let pp_quote fmt x = Format.fprintf fmt "%S" x

  module Set = String.Set
  module Map = String.Map
  module Top_closure = Top_closure.String
end

module Syntax = struct
  type t = OCaml | Reason
end

module File = struct
  type t =
    { path   : Path.t
    ; syntax : Syntax.t
    }

  let make syntax path = { syntax; path }
end

type t =
  { name     : Name.t
  ; impl     : File.t option
  ; intf     : File.t option
  ; obj_name : string
  ; pp       : (unit, string list) Build.t option
  }

let name t = t.name

let make ?impl ?intf ?obj_name name =
  let file : File.t =
    match impl, intf with
    | None, None ->
      Exn.code_error "Module.make called with no files"
        [ "name", Sexp.To_sexp.string name
        ; "impl", Sexp.To_sexp.(option unknown) impl
        ; "intf", Sexp.To_sexp.(option unknown) intf
        ]
    | Some file, _
    | _, Some file -> file
  in
  let obj_name =
    match obj_name with
    | Some s -> s
    | None ->
      let fn = Path.basename file.path in
      match String.index fn '.' with
      | None   -> fn
      | Some i -> String.sub fn ~pos:0 ~len:i
  in
  { name
  ; impl
  ; intf
  ; obj_name
  ; pp = None
  }

let real_unit_name t = Name.of_string (Filename.basename t.obj_name)

let has_impl t = Option.is_some t.impl

let file t (kind : Ml_kind.t) =
  let file =
    match kind with
    | Impl -> t.impl
    | Intf -> t.intf
  in
  Option.map file ~f:(fun f -> f.path)

let obj_file t ~obj_dir ~ext = Path.relative obj_dir (t.obj_name ^ ext)

let cm_source t kind = file t (Cm_kind.source kind)

let cm_file_unsafe t ~obj_dir kind =
  obj_file t ~obj_dir ~ext:(Cm_kind.ext kind)

let cm_file t ~obj_dir (kind : Cm_kind.t) =
  match kind with
  | (Cmx | Cmo) when not (has_impl t) -> None
  | _ -> Some (cm_file_unsafe t ~obj_dir kind)

let cmt_file t ~obj_dir (kind : Ml_kind.t) =
  match kind with
  | Impl -> Option.map t.impl ~f:(fun _ -> obj_file t ~obj_dir ~ext:".cmt" )
  | Intf -> Option.map t.intf ~f:(fun _ -> obj_file t ~obj_dir ~ext:".cmti")

let odoc_file t ~doc_dir = obj_file t ~obj_dir:doc_dir~ext:".odoc"

let cmti_file t ~obj_dir =
  match t.intf with
  | None   -> obj_file t ~obj_dir ~ext:".cmt"
  | Some _ -> obj_file t ~obj_dir ~ext:".cmti"

let iter t ~f =
  Option.iter t.impl ~f:(f Ml_kind.Impl);
  Option.iter t.intf ~f:(f Ml_kind.Intf)

let with_wrapper t ~libname =
  { t with obj_name = sprintf "%s__%s" libname t.name }

let map_files t ~f =
  { t with
    impl = Option.map t.impl ~f:(f Ml_kind.Impl)
  ; intf = Option.map t.intf ~f:(f Ml_kind.Intf)
  }

let dir t =
  let file =
    match t.intf with
    | Some x -> x
    | None -> Option.value_exn t.impl
  in
  Path.parent_exn file.path

let set_pp t pp = { t with pp }
