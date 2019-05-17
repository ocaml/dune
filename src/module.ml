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

  (* XXX this will need to be fixed once (include_subdirs qualified) is
     supported *)
  let split_alias_prefix t =
    let len = String.length t in
    let rec loop t i =
      if i >= len - 1 then
        None
      else if t.[i] = '_' && t.[i + 1] = '_' then
        if i = 1 || i = len - 2 then
          None (* name of the form __Foo or Foo__ *)
        else
          Some ( of_string (String.take t i)
               , of_string (String.drop t (i + 2))
               )
      else
        loop t (i + 1)
    in
    loop t 0

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

module Kind = struct
  type t =
    | Intf_only
    | Virtual
    | Impl

  let to_string = function
    | Intf_only -> "intf_only"
    | Virtual -> "virtual"
    | Impl -> "impl"

  let pp fmt t = Format.pp_print_string fmt (to_string t)

  let to_sexp t = Sexp.Encoder.string (to_string t)

  let encode =
    let open Dune_lang.Encoder in
    function
    | Intf_only -> string "intf_only"
    | Virtual -> string "virtual"
    | Impl -> string "impl"

  let decode =
    let open Stanza.Decoder in
    enum
      [ "intf_only", Intf_only
      ; "virtual", Virtual
      ; "impl", Impl
      ]

  let has_impl = function
    | Impl -> true
    | Intf_only
    | Virtual -> false

  let is_virtual = function
    | Virtual -> true
    | _ -> false
end

type t =
  { name       : Name.t
  ; impl       : File.t option
  ; intf       : File.t option
  ; obj_name   : string
  ; pp         : (unit, string list) Build.t option
  ; visibility : Visibility.t
  ; obj_dir    : Obj_dir.t
  ; kind       : Kind.t
  }

let name t = t.name
let pp_flags t = t.pp
let intf t = t.intf
let impl t = t.impl
let kind t = Kind.to_string t.kind
let obj_dir t = t.obj_dir

let make ?impl ?intf ?obj_name ~visibility ~obj_dir ~(kind : Kind.t) name =
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
  begin match kind, impl, intf with
  | Virtual, Some _, _
  | Impl, None, _
  | Intf_only, Some _, _ ->
    let open Sexp.Encoder in
    Exn.code_error "Module.make: invalid kind, impl, intf combination"
      [ "name", Name.to_sexp name
      ; "kind", Kind.to_sexp kind
      ; "intf", (option File.to_sexp) intf
      ; "impl", (option File.to_sexp) impl
      ]
  | _, _ , _ -> ()
  end;
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
  ; kind
  }

let real_unit_name t = Name.of_string (Filename.basename t.obj_name)

let has_impl t = Kind.has_impl t.kind
let has_intf t = Option.is_some t.intf

let impl_only t = has_impl t && not (has_intf t)
let intf_only t = has_intf t && not (has_impl t)

let is_public t = Visibility.is_public t.visibility
let is_private t = Visibility.is_private t.visibility
let is_virtual t = Kind.is_virtual t.kind

let file t (kind : Ml_kind.t) =
  let file =
    match kind with
    | Impl -> t.impl
    | Intf -> t.intf
  in
  Option.map file ~f:(fun f -> f.path)

let obj_file t ~kind ~ext =
  let base = Obj_dir.cm_dir t.obj_dir kind t.visibility in
  Path.relative base (t.obj_name ^ ext)

let obj_name t = t.obj_name

let cm_source t kind = file t (Cm_kind.source kind)

let cm_file_unsafe t ?ext kind =
  let ext = Option.value ext ~default:(Cm_kind.ext kind) in
  obj_file t ~kind ~ext

let cm_file t ?ext (kind : Cm_kind.t) =
  match kind with
  | (Cmx | Cmo) when not (has_impl t) -> None
  | _ -> Some (cm_file_unsafe t ?ext kind)

let cm_public_file_unsafe t ?ext kind =
  let ext = Option.value ext ~default:(Cm_kind.ext kind) in
  let base = Obj_dir.cm_public_dir t.obj_dir kind in
  Path.relative base (t.obj_name ^ ext)

let cm_public_file t ?ext (kind : Cm_kind.t) =
  match kind with
  | (Cmx | Cmo) when not (has_impl t) -> None
  |  Cmi when is_private t -> None
  | _ -> Some (cm_public_file_unsafe t ?ext kind)

let cmt_file t (kind : Ml_kind.t) =
  match kind with
  | Impl -> Option.map t.impl ~f:(fun _ -> obj_file t ~kind:Cmi ~ext:".cmt" )
  | Intf -> Option.map t.intf ~f:(fun _ -> obj_file t ~kind:Cmi ~ext:".cmti")

let odoc_file t ~doc_dir =
  let base =
    match t.visibility with
    | Public -> doc_dir
    | Private -> Utils.library_private_dir ~obj_dir:doc_dir
  in
  Path.relative base (t.obj_name ^ ".odoc")

let cmti_file t =
  match t.intf with
  | None   -> obj_file t ~kind:Cmi ~ext:".cmt"
  | Some _ -> obj_file t ~kind:Cmi ~ext:".cmti"

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

let to_sexp { name; impl; intf; obj_name ; pp ; visibility; obj_dir ; kind } =
  let open Sexp.Encoder in
  record
    [ "name", Name.to_sexp name
    ; "obj_name", string obj_name
    ; "impl", (option File.to_sexp) impl
    ; "intf", (option File.to_sexp) intf
    ; "pp", (option string) (Option.map ~f:(fun _ -> "has pp") pp)
    ; "visibility", Visibility.to_sexp visibility
    ; "obj_dir", Obj_dir.to_sexp obj_dir
    ; "kind", Kind.to_sexp kind
    ]

let pp fmt { name; impl; intf; obj_name ; pp = _ ; visibility; obj_dir ; kind } =
  Fmt.record fmt
    [ "name", Fmt.const Name.pp name
    ; "impl", Fmt.const (Fmt.optional File.pp) impl
    ; "intf", Fmt.const (Fmt.optional File.pp) intf
    ; "obj_name", Fmt.const Format.pp_print_string obj_name
    ; "visibility", Fmt.const Visibility.pp visibility
    ; "obj_dir", Fmt.const Obj_dir.pp obj_dir
    ; "kind", Fmt.const Kind.pp kind
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

  let pp fmt t =
    Fmt.ocaml_list Name.pp fmt (Name.Map.keys t)
end

let set_private t =
  { t with visibility = Private }

let set_obj_dir t ~obj_dir =
  { t with obj_dir }

let set_virtual t =
  { t with kind = Virtual }


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
       ; obj_dir = _
       ; kind
       } as t) =
  let open Dune_lang.Encoder in
  let has_impl = has_impl t in

  let kind =
    match kind with
    | Kind.Impl when has_impl -> None
    | Intf_only when not has_impl -> None
    | Impl | Virtual | Intf_only -> Some kind
  in
  record_fields
    [ field "name" Name.encode name
    ; field "obj_name" string obj_name
    ; field "visibility" Visibility.encode visibility
    ; field_o "kind" Kind.encode kind
    ; field_b "impl" has_impl
    ; field_b "intf" (has_intf t)
    ]

let decode ~obj_dir =
  let open Dune_lang.Decoder in
  let dir = Obj_dir.dir obj_dir in
  fields (
    let+ name = field "name" Name.decode
    and+ obj_name = field "obj_name" string
    and+ visibility = field "visibility" Visibility.decode
    and+ kind = field_o "kind" Kind.decode
    and+ impl = field_b "impl"
    and+ intf = field_b "intf"
    in
    let file exists ml_kind =
      if exists then
        let basename = Name.basename name ~ml_kind ~syntax:OCaml in
        Some (File.make Syntax.OCaml (Path.relative dir basename))
      else
        None
    in
    let kind = match kind with
      | Some k -> k
      | None when impl -> Impl
      | None -> Intf_only
    in
    let intf = file intf Intf in
    let impl = file impl Impl in
    make ~obj_name ~visibility ?impl ?intf ~obj_dir ~kind name
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

  let name t = t.name

  let src_dir t =
  match t.intf, t.impl with
  | None, None -> None
  | Some x, Some _
  | Some x, None
  | None, Some x -> Some (Path.parent_exn x.path)

end

let pped =
  let pped_path path ~suffix =
    (* We need to insert the suffix before the extension as some tools
       inspect the extension *)
    let base, ext = Path.split_extension path in
    Path.extend_basename base ~suffix:(suffix ^ ext)
  in
  map_files ~f:(fun _kind file ->
    let pp_path = pped_path file.path ~suffix:".pp" in
    { file with path = pp_path })

let ml_source =
  map_files ~f:(fun _ f ->
    match f.syntax with
    | OCaml  -> f
    | Reason ->
      let path =
        let base, ext = Path.split_extension f.path in
        let suffix =
          match ext with
          | ".re"  -> ".re.ml"
          | ".rei" -> ".re.mli"
          | _     ->
            Errors.fail
              (Loc.in_file (Path.source (Path.drop_build_context_exn f.path)))
              "Unknown file extension for reason source file: %S"
              ext
        in
        Path.extend_basename base ~suffix
      in
      File.make OCaml path)
