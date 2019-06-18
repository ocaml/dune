open! Stdune
open Import

module Syntax = struct
  type t = OCaml | Reason

  let to_string = function
    | OCaml -> "ocaml"
    | Reason -> "reason"

  let to_dyn t = Dyn.Encoder.string (to_string t)
end

module Name = struct
  module T = struct
    type t = string
    let compare = compare
  end

  include T

  let decode = Dune_lang.Decoder.string
  let encode = Dune_lang.Encoder.string

  let to_dyn = Dyn.Encoder.string

  let add_suffix = (^)

  let of_string = String.capitalize
  let to_string x = x

  let uncapitalize = String.uncapitalize

  let pp = Format.pp_print_string
  let pp_quote fmt x = Format.fprintf fmt "%S" x

  module Set = struct
    include String.Set
    let to_dyn t = Dyn.Set (List.map ~f:(fun s -> Dyn.String s) (to_list t))
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

  let path t = t.path

  let set_src_dir t ~src_dir =
    let path = Path.relative src_dir (Path.basename t.path) in
    { t with path }

  let make syntax path = { syntax; path }

  let to_dyn { path; syntax } =
    let open Dyn.Encoder in
    record
      [ "path", Path.to_dyn path
      ; "syntax", Syntax.to_dyn syntax
      ]

end

module Kind = struct
  type t =
    | Intf_only
    | Virtual
    | Impl
    | Alias
    | Impl_vmodule

  let to_string = function
    | Intf_only -> "intf_only"
    | Virtual -> "virtual"
    | Impl -> "impl"
    | Alias -> "alias"
    | Impl_vmodule -> "impl_vmodule"

  let to_dyn t = Dyn.Encoder.string (to_string t)

  let encode t = Dune_lang.Encoder.string (to_string t)

  let decode =
    let open Stanza.Decoder in
    enum
      [ "intf_only", Intf_only
      ; "virtual", Virtual
      ; "impl", Impl
      ; "alias", Alias
      ; "impl_vmodule", Impl_vmodule
      ]

  let has_impl = function
    | Alias
    | Impl_vmodule
    | Impl -> true
    | Intf_only
    | Virtual -> false
end

(* Only the source of a module, not yet associated to a library *)
module Source = struct
  type t =
    { name : Name.t
    ; files : File.t option Ml_kind.Dict.t
    }

  let to_dyn { name; files } =
    let open Dyn.Encoder in
    record
      [ "name", Name.to_dyn name
      ; "files", Ml_kind.Dict.to_dyn (option File.to_dyn) files
      ]

  let make ?impl ?intf name =
    begin match impl, intf with
    | None, None ->
      Code_error.raise "Module.Source.make called with no files"
        [ "name", Dyn.Encoder.string name
        ]
    | Some _, _
    | _, Some _ -> ()
    end;
    let files = Ml_kind.Dict.make ~impl ~intf in
    { name
    ; files
    }

  let has_impl t = Option.is_some t.files.impl

  let name t = t.name

  let choose_file { files = {impl; intf}; name = _} =
    match intf, impl with
    | None, None -> assert false
    | Some x, Some _
    | Some x, None
    | None, Some x -> x

  let src_dir t = Path.parent_exn (choose_file t).path

  let map_files t ~f =
    let files = Ml_kind.Dict.mapi ~f t.files in
    { t with files }
end

type t =
  { source     : Source.t
  ; obj_name   : string
  ; pp         : (unit, string list) Build.t option
  ; visibility : Visibility.t
  ; kind       : Kind.t
  }

let name t = t.source.name
let kind t = t.kind
let pp_flags t = t.pp

let of_source ?obj_name ~visibility ~(kind : Kind.t)
      (source : Source.t) =
  begin match kind, source.files.impl, source.files.intf with
  | (Alias | Impl_vmodule | Impl), None, _
  | (Alias | Impl_vmodule), Some _, Some _
  | (Intf_only | Virtual), Some _, _
  | (Intf_only | Virtual), _, None ->
    let open Dyn.Encoder in
    Code_error.raise "Module.make: invalid kind, impl, intf combination"
      [ "name", Name.to_dyn source.name
      ; "kind", Kind.to_dyn kind
      ; "intf", (option File.to_dyn) source.files.intf
      ; "impl", (option File.to_dyn) source.files.impl
      ]
  | _, _ , _ -> ()
  end;
  let obj_name =
    match obj_name with
    | Some s -> s
    | None ->
      let file = Source.choose_file source in
      let fn = Path.basename file.path in
      match String.index fn '.' with
      | None   -> fn
      | Some i -> String.take fn i
  in
  { source
  ; obj_name
  ; pp = None
  ; visibility
  ; kind
  }

let real_unit_name t = Name.of_string (Filename.basename t.obj_name)

let has t ~ml_kind =
  match (ml_kind : Ml_kind.t) with
  | Impl -> Kind.has_impl t.kind
  | Intf -> Option.is_some t.source.files.intf

let source t ~(ml_kind : Ml_kind.t) =
  Ml_kind.Dict.get t.source.files ml_kind

let file t ~(ml_kind : Ml_kind.t) =
  source t ~ml_kind
  |> Option.map ~f:File.path

let obj_name t = t.obj_name

let odoc_file t ~doc_dir =
  let base =
    match t.visibility with
    | Public -> doc_dir
    | Private -> Utils.library_private_dir ~obj_dir:doc_dir
  in
  Path.Build.relative base (t.obj_name ^ ".odoc")

let iter t ~f =
  Ml_kind.Dict.iteri t.source.files
    ~f:(fun kind -> Option.iter ~f:(f kind))

let with_wrapper t ~main_module_name =
  { t with obj_name
           = sprintf "%s__%s"
               (String.uncapitalize main_module_name) t.source.name
  }

let map_files t ~f =
  let source = Source.map_files t.source ~f:(fun kind -> Option.map ~f:(f kind)) in
  { t with source }

let src_dir t = Source.src_dir t.source

let set_pp t pp = { t with pp }

let to_dyn { source ; obj_name ; pp ; visibility; kind } =
  let open Dyn.Encoder in
  record
    [ "source", Source.to_dyn source
    ; "obj_name", string obj_name
    ; "pp", (option string) (Option.map ~f:(fun _ -> "has pp") pp)
    ; "visibility", Visibility.to_dyn visibility
    ; "kind", Kind.to_dyn kind
    ]

let ml_gen = ".ml-gen"

let wrapped_compat t =
  let source =
    let impl =
      Some (
        { File.
          syntax = OCaml
        ; path =
            (* Option.value_exn cannot fail because we disallow wrapped
               compatibility mode for virtual libraries. That means none of the
               modules are implementing a virtual module, and therefore all have
               a source dir *)
            Path.L.relative (src_dir t)
              [ ".wrapped_compat"
              ; Name.to_string t.source.name ^ ml_gen
              ]
        }
      )
    in
    { t.source with
      files = { intf = None ; impl }
    }
  in
  { t with source }

let visibility t = t.visibility

let sources t =
  List.filter_map [t.source.files.intf; t.source.files.impl]
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
          Code_error.raise "top_closure: unable to find key"
            [ "m", to_dyn m
            ; "t", (Dyn.Encoder.list to_dyn) (keys t)
            ])
end

let encode
      ({ source = { name ; files = _}
       ; obj_name
       ; pp = _
       ; visibility
       ; kind
       } as t) =
  let open Dune_lang.Encoder in
  let has_impl = has t ~ml_kind:Impl in

  let kind =
    match kind with
    | Kind.Impl when has_impl -> None
    | Intf_only when not has_impl -> None
    | Impl_vmodule | Alias | Impl | Virtual | Intf_only -> Some kind
  in
  record_fields
    [ field "name" Name.encode name
    ; field "obj_name" string obj_name
    ; field "visibility" Visibility.encode visibility
    ; field_o "kind" Kind.encode kind
    ; field_b "impl" has_impl
    ; field_b "intf" (has t ~ml_kind:Intf)
    ]

let decode ~src_dir =
  let open Dune_lang.Decoder in
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
        Some (File.make Syntax.OCaml (Path.relative src_dir basename))
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
    let source = Source.make ?impl ?intf name in
    of_source ~obj_name ~visibility ~kind source
  )

let pped =
  let pped_path path ~suffix =
    (* We need to insert the suffix before the extension as some tools
       inspect the extension *)
    let base, ext = Path.split_extension path in
    Path.extend_basename base ~suffix:(suffix ^ ext)
  in
  map_files ~f:(fun _kind (file : File.t) ->
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

let set_src_dir t ~src_dir =
  map_files t ~f:(fun _ -> File.set_src_dir ~src_dir)

let generated ~src_dir name =
  let basename = String.uncapitalize (Name.to_string name) in
  let source =
    let impl =
      File.make OCaml (Path.relative src_dir (basename ^ ml_gen)) in
    Source.make ~impl name in
  of_source
    ~visibility:Public
    ~kind:Impl
    ~obj_name:basename
    source

let generated_alias ~src_dir name =
  let t = generated ~src_dir name in
  { t with kind = Alias }

module Name_map = struct
  type nonrec t = t Name.Map.t

  let impl_only =
    Name.Map.fold ~init:[] ~f:(fun m acc ->
      if has m ~ml_kind:Impl then
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
