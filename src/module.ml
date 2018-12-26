open! Stdune
open Import

module Syntax = struct
  type t = OCaml | Reason

  let to_string = function
    | OCaml -> "ocaml"
    | Reason -> "reason"

  let pp fmt t = Format.pp_print_string fmt (to_string t)

  let to_sexp t = Sexp.Encoder.string (to_string t)
end

module Name = struct
  module T = struct
    type t = string
    let compare = compare
  end

  include T

  let decode = Dune_lang.Decoder.string
  let encode = Dune_lang.Encoder.string

  let to_sexp = Sexp.Encoder.string

  let add_suffix = (^)

  let of_string = String.capitalize
  let to_string x = x

  let uncapitalize = String.uncapitalize

  let pp = Format.pp_print_string
  let pp_quote fmt x = Format.fprintf fmt "%S" x

  module Set = struct
    include String.Set
  end
  module Map = String.Map
  module Top_closure = Top_closure.String
  module Infix = Comparable.Operators(T)

  let of_local_lib_name s =
    of_string (Lib_name.Local.to_string s)

  let to_local_lib_name s =
    Lib_name.Local.of_string_exn s

  let basename n ~(ml_kind : Ml_kind.t) ~(syntax : Syntax.t) =
    let ext =
      match syntax, ml_kind with
      | Reason, Intf -> ".rei"
      | Reason, Impl -> ".re"
      | OCaml, Intf -> ".mli"
      | OCaml, Impl -> ".ml"
    in
    String.lowercase n ^ ext

  let strip_alias_prefix t =
    let len = String.length t in
    let rec loop t i =
      if i <= 2 then
        t
      else if t.[i - 2] = '_' && t.[i - 1] = '_' then
        String.sub t ~pos:i ~len:(len - i)
      else
        loop t (i - 1)
    in
    let name = loop t (len - 1) in
    if Char.uppercase_ascii name.[0] = name.[0] then
      name
    else
      die "Invalid module name: %S" name
end

module File = struct
  type t =
    { path   : Path.t
    ; syntax : Syntax.t
    }

  let make syntax path = { syntax; path }

  let to_sexp { path; syntax } =
    let open Sexp.Encoder in
    record
      [ "path", Path.to_sexp path
      ; "syntax", Syntax.to_sexp syntax
      ]

  let pp fmt { path; syntax } =
    Fmt.record fmt
      [ "path", Fmt.const Path.pp path
      ; "syntax", Fmt.const Syntax.pp syntax
      ]
end

module Visibility = struct
  type t = Public | Private

  let to_string = function
    | Public -> "public"
    | Private -> "private"

  let pp fmt t = Format.pp_print_string fmt (to_string t)

  let to_sexp t = Sexp.Encoder.string (to_string t)

  let encode =
    let open Dune_lang.Encoder in
    function
    | Public -> string "public"
    | Private -> string "private"

  let decode =
    let open Dune_lang.Decoder in
    plain_string (fun ~loc -> function
      | "public" -> Public
      | "private" -> Private
      | _ -> Errors.fail loc
               "Not a valid visibility. Valid visibility is public or private")

  let is_public = function
    | Public -> true
    | Private -> false

  let is_private t = not (is_public t)
end

type t =
  { name       : Name.t
  ; impl       : File.t option
  ; intf       : File.t option
  ; obj_name   : string
  ; pp         : (unit, string list) Build.t option
  ; visibility : Visibility.t
  ; obj_dir    : Obj_dir.t
  }

let name t = t.name
let pp_flags t = t.pp
let intf t = t.intf
let impl t = t.impl
let obj_dir t = t.obj_dir

let make ?impl ?intf ?obj_name ~visibility ~obj_dir name =
  let file : File.t =
    match impl, intf with
    | None, None ->
      Exn.code_error "Module.make called with no files"
        [ "name", Sexp.Encoder.string name
        ; "impl", Sexp.Encoder.(option unknown) impl
        ; "intf", Sexp.Encoder.(option unknown) intf
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
      | Some i -> String.take fn i
  in
  { name
  ; impl
  ; intf
  ; obj_name
  ; pp = None
  ; visibility
  ; obj_dir
  }

let real_unit_name t = Name.of_string (Filename.basename t.obj_name)

let has_impl t = Option.is_some t.impl
let has_intf t = Option.is_some t.intf

let impl_only t = has_impl t && not (has_intf t)
let intf_only t = has_intf t && not (has_impl t)

let file t (kind : Ml_kind.t) =
  let file =
    match kind with
    | Impl -> t.impl
    | Intf -> t.intf
  in
  Option.map file ~f:(fun f -> f.path)

let obj_file t ~ext =
  let base =
    match t.visibility with
    | Public -> t.obj_dir.public_dir
    | Private -> Option.value_exn t.obj_dir.private_dir
  in
  Path.relative base (t.obj_name ^ ext)

let obj_name t = t.obj_name

let cm_source t kind = file t (Cm_kind.source kind)

let cm_file_unsafe t ?ext kind =
  obj_file t ~ext:(Option.value ~default:(Cm_kind.ext kind) ext)

let cm_file t (kind : Cm_kind.t) =
  match kind with
  | (Cmx | Cmo) when not (has_impl t) -> None
  | _ -> Some (cm_file_unsafe t kind)

let cmt_file t (kind : Ml_kind.t) =
  match kind with
  | Impl -> Option.map t.impl ~f:(fun _ -> obj_file t ~ext:".cmt" )
  | Intf -> Option.map t.intf ~f:(fun _ -> obj_file t ~ext:".cmti")

let odoc_file t ~doc_dir =
  let base =
    match t.visibility with
    | Public -> doc_dir
    | Private -> Utils.library_private_obj_dir ~obj_dir:doc_dir
  in
  Path.relative base (t.obj_name ^ ".odoc")

let cmti_file t =
  match t.intf with
  | None   -> obj_file t ~ext:".cmt"
  | Some _ -> obj_file t ~ext:".cmti"

let iter t ~f =
  Option.iter t.impl ~f:(f Ml_kind.Impl);
  Option.iter t.intf ~f:(f Ml_kind.Intf)

let with_wrapper t ~main_module_name =
  { t with obj_name
           = sprintf "%s__%s"
               (String.uncapitalize main_module_name) t.name
  }

let map_files t ~f =
  { t with
    impl = Option.map t.impl ~f:(f Ml_kind.Impl)
  ; intf = Option.map t.intf ~f:(f Ml_kind.Intf)
  }

let src_dir t =
  match t.intf, t.impl with
  | None, None -> None
  | Some x, Some _
  | Some x, None
  | None, Some x -> Some (Path.parent_exn x.path)

let set_pp t pp = { t with pp }

let to_sexp { name; impl; intf; obj_name ; pp ; visibility; obj_dir } =
  let open Sexp.Encoder in
  record
    [ "name", Name.to_sexp name
    ; "obj_name", string obj_name
    ; "impl", (option File.to_sexp) impl
    ; "intf", (option File.to_sexp) intf
    ; "pp", (option string) (Option.map ~f:(fun _ -> "has pp") pp)
    ; "visibility", Visibility.to_sexp visibility
    ; "obj_dir", Obj_dir.to_sexp obj_dir
    ]

let pp fmt { name; impl; intf; obj_name ; pp = _ ; visibility; obj_dir } =
  Fmt.record fmt
    [ "name", Fmt.const Name.pp name
    ; "impl", Fmt.const (Fmt.optional File.pp) impl
    ; "intf", Fmt.const (Fmt.optional File.pp) intf
    ; "obj_name", Fmt.const Format.pp_print_string obj_name
    ; "visibility", Fmt.const Visibility.pp visibility
    ; "obj_dir", Fmt.const Obj_dir.pp obj_dir
    ]

let wrapped_compat t =
  { t with
    intf = None
  ; impl =
      Some (
        { syntax = OCaml
        ; path =
            (* Option.value_exn cannot fail because we disallow wrapped
               compatibility mode for virtual libraries. That means none of the
               modules are implementing a virtual module, and therefore all have
               a source dir *)
            Path.L.relative (Option.value_exn (src_dir t))
              [ ".wrapped_compat"
              ; Name.to_string t.name ^ ".ml-gen"
              ]
        }
      )
  }

module Name_map = struct
  type nonrec t = t Name.Map.t

  let impl_only =
    Name.Map.fold ~init:[] ~f:(fun m acc ->
      if has_impl m then
        m :: acc
      else
        acc)

  let of_list_exn modules =
    List.map modules ~f:(fun m -> (name m, m))
    |> Name.Map.of_list_exn

  let add t module_ =
    Name.Map.add t (name module_) module_
end

let is_public t = Visibility.is_public t.visibility
let is_private t = Visibility.is_private t.visibility

let set_private t =
  { t with visibility = Private }
let set_obj_dir ~obj_dir t =
  { t with obj_dir }

let visibility t = t.visibility

let remove_files t =
  { t with
    intf = None
  ; impl = None
  }

let sources t =
  List.filter_map [t.intf; t.impl]
    ~f:(Option.map ~f:(fun (x : File.t) -> x.path))

module Obj_map = struct
  include Map.Make(struct
      type nonrec t = t
      let compare m1 m2 = String.compare m1.obj_name m2.obj_name
    end)

  let top_closure t =
    Top_closure.String.top_closure
      ~key:(fun m -> m.obj_name)
      ~deps:(fun m ->
        match find t m with
        | Some m -> m
        | None ->
          Exn.code_error "top_closure: unable to find key"
            [ "m", to_sexp m
            ; "t", (Sexp.Encoder.list to_sexp) (keys t)
            ])
end

let encode
      ({ name
       ; impl = _
       ; intf = _
       ; obj_name
       ; pp = _
       ; visibility
       ; obj_dir
       } as t) =
  let open Dune_lang.Encoder in
  record_fields
    [ field "name" Name.encode name
    ; field "obj_name" string obj_name
    ; field "visibility" Visibility.encode visibility
    ; field_b "impl" (has_impl t)
    ; field_b "intf" (has_intf t)
    ; field_i "obj_dir" Obj_dir.encode obj_dir
    ]

let decode ~dir =
  let open Dune_lang.Decoder in
  fields (
    let%map name = field "name" Name.decode
    and obj_name = field "obj_name" string
    and visibility = field "visibility" Visibility.decode
    and impl = field_b "impl"
    and intf = field_b "intf"
    and obj_dir = field ~default:(Obj_dir.make_external ~dir) "obj_dir" (Obj_dir.decode ~dir)
    in
    let file exists ml_kind =
      if exists then
        let basename = Name.basename name ~ml_kind ~syntax:OCaml in
        Some (File.make Syntax.OCaml (Path.relative dir basename))
      else
        None
    in
    let intf = file intf Intf in
    let impl = file impl Impl in
    make ~obj_name ~visibility ?impl ?intf ~obj_dir name
  )


(* Only the source of a module, not yet associated to a library *)
module Source = struct
  type t =
    { name : Name.t
    ; impl : File.t option
    ; intf : File.t option
    }

  let make ?impl ?intf name =
    begin match impl, intf with
    | None, None ->
      Exn.code_error "Module.Source.make called with no files"
        [ "name", Sexp.Encoder.string name
        ; "impl", Sexp.Encoder.(option unknown) impl
        ; "intf", Sexp.Encoder.(option unknown) intf
        ]
    | Some _, _
    | _, Some _ -> ()
    end;
    { name
    ; impl
    ; intf
    }

  let has_impl t = Option.is_some t.impl

  let src_dir t =
  match t.intf, t.impl with
  | None, None -> None
  | Some x, Some _
  | Some x, None
  | None, Some x -> Some (Path.parent_exn x.path)

end
