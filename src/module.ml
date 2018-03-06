open Import

module Name = struct
  type t = string

  let t = Sexp.atom

  let add_suffix = (^)

  let compare = compare
  let of_string = String.capitalize
  let to_string x = x

  let pp = Format.pp_print_string
  let pp_quote fmt x = Format.fprintf fmt "%S" x

  module Set = String_set
  module Map = String_map
end

module Syntax = struct
  type t = OCaml | Reason
end

module File = struct
  type t =
    { name : string
    ; syntax : Syntax.t
    }

  let to_ocaml t =
    match t.syntax with
    | OCaml -> code_errorf "to_ocaml: can only convert reason Files" ()
    | Reason ->
      { syntax = OCaml
      ; name =
          let base, ext = Filename.split_extension t.name in
          base ^ ".re" ^
          (match Filename.extension t.name with
           | ".re" -> ".ml"
           | ".rei" -> ".mli"
           | _ -> code_errorf "to_ocaml: unrecognized extension %s" ext ())
      }
end

type t =
  { name     : Name.t
  ; impl     : File.t option
  ; intf     : File.t option
  ; obj_name : string
  }

let name t = t.name

let real_unit_name t = Name.of_string (Filename.basename t.obj_name)

let has_impl t = Option.is_some t.impl

let file t ~dir (kind : Ml_kind.t) =
  let file =
    match kind with
    | Impl -> t.impl
    | Intf -> t.intf
  in
  Option.map file ~f:(fun f -> Path.relative dir f.name)

let obj_file t ~obj_dir ~ext = Path.relative obj_dir (t.obj_name ^ ext)

let cm_source t ~dir kind = file t ~dir (Cm_kind.source kind)

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

let set_obj_name t ~wrapper =
  match wrapper with
  | Some s -> { t with obj_name = sprintf "%s__%s" s t.name }
  | None ->
    let fn =
      match t.impl with
      | Some f -> f.name
      | None -> (Option.value_exn t.intf).name
    in
    let obj_name  =
      match String.index fn '.' with
      | None -> fn
      | Some i -> String.sub fn ~pos:0 ~len:i
    in
    { t with obj_name }
