open Import
open Sexp.Of_sexp

(* This file defines the jbuild types as well as the S-expression syntax for the various
   supported version of the specification.

   [vN] is for the version [N] of the specification and [vjs] is for the rolling
   [jane_street] version, when needed.
*)

module Jbuild_version = struct
  type t =
    | V1

  let t =
    enum
      [ "1", V1
      ]

  let latest_stable = V1
end

let invalid_module_name name sexp =
  of_sexp_error sexp (sprintf "invalid module name: %S" name)

let module_name sexp =
  let name = string sexp in
  match name with
  | "" -> invalid_module_name name sexp
  | s ->
    (match s.[0] with
     | 'A'..'Z' | 'a'..'z' -> ()
     | _ -> invalid_module_name name sexp);
    String.iter s ~f:(function
      | 'A'..'Z' | 'a'..'z' | '0'..'9' | '\'' | '_' -> ()
      | _ -> invalid_module_name name sexp);
    String.capitalize s

let module_names sexp = String.Set.of_list (list module_name sexp)

let invalid_lib_name sexp =
  of_sexp_error sexp "invalid library name"

let library_name sexp =
  match string sexp with
  | "" -> invalid_lib_name sexp
  | s ->
    if s.[0] = '.' then invalid_lib_name sexp;
    String.iter s ~f:(function
      | 'A'..'Z' | 'a'..'z' | '_' | '.' | '0'..'9' -> ()
      | _ -> invalid_lib_name sexp);
    s

let file sexp =
  match string sexp with
  | "." | ".." ->
    of_sexp_error sexp "'.' and '..' are not valid filenames"
  | fn -> fn

let file_in_current_dir sexp =
  match string sexp with
  | "." | ".." ->
    of_sexp_error sexp "'.' and '..' are not valid filenames"
  | fn ->
    if Filename.dirname fn <> Filename.current_dir_name then
      of_sexp_error sexp "file in current directory expected";
    fn

let relative_file sexp =
  let fn = file sexp in
  if not (Filename.is_relative fn) then
    of_sexp_error sexp "relative filename expected";
  fn

let c_name, cxx_name =
  let make what ext sexp =
    let s = string sexp in
    if match s with
      | "" | "." | ".."  -> true
      | _ -> Filename.basename s <> s then
      of_sexp_errorf sexp
        "%S is not a valid %s name.\n\
         Hint: To use %s files from another directory, use a \
         (copy_files <dir>/*.c) stanza instead."
        s what what ext
    else
      s
  in
  (make "C"   "c",
   make "C++" "cpp")

(* Parse and resolve "package" fields *)
module Pkg = struct
  let listing packages =
    let longest_pkg =
      String.longest_map packages ~f:(fun p ->
        Package.Name.to_string p.Package.name)
    in
    String.concat ~sep:"\n"
      (List.map packages ~f:(fun pkg ->
         sprintf "- %-*s (because of %s)" longest_pkg
           (Package.Name.to_string pkg.Package.name)
           (Path.to_string (Package.opam_file pkg))))

  let default (project : Dune_project.t) =
    match Package.Name.Map.values project.packages with
    | [pkg] -> Ok pkg
    | [] ->
      Error
        "The current project (%S declared in  defines no packages.\n\
         What do you want me to do with this (install ...) stanzas?.\n\
         You need to add a <package>.opam file at the root \
         of your project so that\n\
         I know that you want to install things as part of package <package>."
    | _ :: _ :: _ ->
      Error
        (sprintf
           "I can't determine automatically which package this (install ...) \
            stanza is for. I have the choice between these ones:\n\
            %s\n\
            You need to add a (package ...) field in this (install ...) stanza"
           (listing (Package.Name.Map.values project.packages)))

  let resolve (project : Dune_project.t) name =
    match Package.Name.Map.find project.packages name with
    | Some pkg ->
      Ok pkg
    | None ->
      let name_s = Package.Name.to_string name in
      if Package.Name.Map.is_empty project.packages then
        Error (sprintf
                 "You cannot declare items to be installed without \
                  adding a <package>.opam file at the root of your project.\n\
                  To declare elements to be installed as part of package %S, \
                  add a %S file at the root of your project."
                 name_s (Package.Name.opam_fn name))
      else
        Error (sprintf
                 "The current scope doesn't define package %S.\n\
                  The only packages for which you can declare \
                  elements to be installed in this directory are:\n\
                  %s%s"
                 name_s
                 (listing (Package.Name.Map.values project.packages))
                 (hint name_s (Package.Name.Map.keys project.packages
                               |> List.map ~f:Package.Name.to_string)))

  let t p sexp =
    match resolve p (Package.Name.of_string (string sexp)) with
    | Ok p -> p
    | Error s -> Loc.fail (Sexp.Ast.loc sexp) "%s" s

  let field p =
    map_validate (field_o "package" string) ~f:(function
      | None -> default p
      | Some name -> resolve p (Package.Name.of_string name))
end

module Pp : sig
  type t = private string
  val of_string : string -> t
  val to_string : t -> string
  val compare : t -> t -> Ordering.t
end = struct
  type t = string

  let of_string s =
    assert (not (String.is_prefix s ~prefix:"-"));
    s

  let to_string t = t

  let compare = String.compare
end

module Pp_or_flags = struct
  type t =
    | PP of Loc.t * Pp.t
    | Flags of string list

  let of_string ~loc s =
    if String.is_prefix s ~prefix:"-" then
      Flags [s]
    else
      PP (loc, Pp.of_string s)

  let t = function
    | Atom (loc, A s) | Quoted_string (loc, s) -> of_string ~loc s
    | List (_, l) -> Flags (List.map l ~f:string)

  let split l =
    let pps, flags =
      List.partition_map l ~f:(function
        | PP (loc, pp) -> Left (loc, pp)
        | Flags s      -> Right s)
    in
    (pps, List.concat flags)
end

module Dep_conf = struct
  type t =
    | File of String_with_vars.t
    | Alias of String_with_vars.t
    | Alias_rec of String_with_vars.t
    | Glob_files of String_with_vars.t
    | Files_recursively_in of String_with_vars.t
    | Package of String_with_vars.t
    | Universe

  let t =
    let t =
      let cstr_sw name f =
        cstr name (String_with_vars.t @> nil) f
      in
      sum
        [ cstr_sw "file"                 (fun x -> File x)
        ; cstr_sw "alias"                (fun x -> Alias x)
        ; cstr_sw "alias_rec"            (fun x -> Alias_rec x)
        ; cstr_sw "glob_files"           (fun x -> Glob_files x)
        ; cstr_sw "files_recursively_in" (fun x -> Files_recursively_in x)
        ; cstr_sw "package"              (fun x -> Package x)
        ; cstr    "universe" nil         Universe
        ]
    in
    fun sexp ->
      match sexp with
      | Atom _ | Quoted_string _ -> File (String_with_vars.t sexp)
      | List _ -> t sexp

  open Sexp
  let sexp_of_t = function
    | File t ->
       List [Sexp.unsafe_atom_of_string "file" ; String_with_vars.sexp_of_t t]
    | Alias t ->
       List [Sexp.unsafe_atom_of_string "alias" ; String_with_vars.sexp_of_t t]
    | Alias_rec t ->
       List [Sexp.unsafe_atom_of_string "alias_rec" ;
             String_with_vars.sexp_of_t t]
    | Glob_files t ->
       List [Sexp.unsafe_atom_of_string "glob_files" ;
             String_with_vars.sexp_of_t t]
    | Files_recursively_in t ->
       List [Sexp.unsafe_atom_of_string "files_recursively_in" ;
             String_with_vars.sexp_of_t t]
    | Package t ->
      List [Sexp.unsafe_atom_of_string "package" ;
            String_with_vars.sexp_of_t t]
    | Universe ->
      Sexp.unsafe_atom_of_string "universe"
end

module Preprocess = struct
  type pps = { pps : (Loc.t * Pp.t) list; flags : string list }
  type t =
    | No_preprocessing
    | Action of Loc.t * Action.Unexpanded.t
    | Pps    of pps

  let t =
    sum
      [ cstr "no_preprocessing" nil No_preprocessing
      ; cstr "action" (located Action.Unexpanded.t @> nil)
          (fun (loc, x) -> Action (loc, x))
      ; cstr "pps" (list Pp_or_flags.t @> nil) (fun l ->
          let pps, flags = Pp_or_flags.split l in
          Pps { pps; flags })
      ]

  let pps = function
    | Pps { pps; _ } -> pps
    | _ -> []
end

module Per_module = struct
  include Per_item.Make(Module.Name)

  let t ~default a sexp =
    match sexp with
    | List (_, Atom (_, A "per_module") :: rest) -> begin
      List.map rest ~f:(fun sexp ->
        let pp, names = pair a module_names sexp in
        (List.map ~f:Module.Name.of_string (String.Set.to_list names), pp))
      |> of_mapping ~default
      |> function
      | Ok t -> t
      | Error (name, _, _) ->
        of_sexp_error sexp (sprintf "module %s present in two different sets"
                              (Module.Name.to_string name))
    end
    | sexp -> for_all (a sexp)
end

module Preprocess_map = struct
  type t = Preprocess.t Per_module.t
  let t = Per_module.t Preprocess.t ~default:Preprocess.No_preprocessing

  let no_preprocessing = Per_module.for_all Preprocess.No_preprocessing

  let find module_name t = Per_module.get t module_name

  let default = Per_module.for_all Preprocess.No_preprocessing

  module Pp_map = Map.Make(Pp)

  let pps t =
    Per_module.fold t ~init:Pp_map.empty ~f:(fun pp acc ->
      List.fold_left (Preprocess.pps pp) ~init:acc ~f:(fun acc (loc, pp) ->
        Pp_map.add acc pp loc))
    |> Pp_map.foldi ~init:[] ~f:(fun pp loc acc -> (loc, pp) :: acc)
end

module Lint = struct
  type t = Preprocess_map.t

  let t = Preprocess_map.t

  let default = Preprocess_map.default
  let no_lint = default
end

let field_oslu name =
  field name Ordered_set_lang.Unexpanded.t
    ~default:Ordered_set_lang.Unexpanded.standard

module Js_of_ocaml = struct

  type t =
    { flags            : Ordered_set_lang.Unexpanded.t
    ; javascript_files : string list
    }

  let t =
    record
      (field_oslu "flags"                                     >>= fun flags ->
       field     "javascript_files" (list string) ~default:[] >>= fun javascript_files ->
       return { flags; javascript_files })

  let default =
    { flags = Ordered_set_lang.Unexpanded.standard
    ; javascript_files = [] }
end

module Lib_dep = struct
  type choice =
    { required  : String.Set.t
    ; forbidden : String.Set.t
    ; file      : string
    }

  type select =
    { result_fn : string
    ; choices   : choice list
    ; loc       : Loc.t (* For error messages *)
    }

  type t =
    | Direct of (Loc.t * string)
    | Select of select

  let choice = function
    | List (_, l) as sexp ->
      let rec loop required forbidden = function
        | [Atom (_, A "->"); fsexp] ->
          let common = String.Set.inter required forbidden in
          Option.iter (String.Set.choose common) ~f:(fun name ->
            of_sexp_errorf sexp
              "library %S is both required and forbidden in this clause"
              name);
          { required
          ; forbidden
          ; file = file fsexp
          }
        | Atom (_, A "->") :: _
        | List _ :: _ | [] ->
          of_sexp_error sexp "(<[!]libraries>... -> <file>) expected"
        | (Atom (_, A s) | Quoted_string (_, s)) :: l ->
          let len = String.length s in
          if len > 0 && s.[0] = '!' then
            let s = String.sub s ~pos:1 ~len:(len - 1) in
            loop required (String.Set.add forbidden s) l
          else
            loop (String.Set.add required s) forbidden l
      in
      loop String.Set.empty String.Set.empty l
    | sexp -> of_sexp_error sexp "(<library-name> <code>) expected"

  let t = function
    | Atom (loc, A s) | Quoted_string (loc, s) ->
      Direct (loc, s)
    | List (loc, Atom (_, A "select") :: m :: Atom (_, A "from") :: libs) ->
      Select { result_fn = file m
             ; choices   = List.map libs ~f:choice
             ; loc
             }
    | sexp ->
      of_sexp_error sexp "<library> or (select <module> from <libraries...>) expected"

  let to_lib_names = function
    | Direct (_, s) -> [s]
    | Select s ->
      List.fold_left s.choices ~init:String.Set.empty ~f:(fun acc x ->
        String.Set.union acc (String.Set.union x.required x.forbidden))
      |> String.Set.to_list

  let direct x = Direct x

  let of_pp (loc, pp) = Direct (loc, Pp.to_string pp)
end

module Lib_deps = struct
  type t = Lib_dep.t list

  type kind =
    | Required
    | Optional
    | Forbidden

  let t sexp =
    let t = list Lib_dep.t sexp in
    let add kind name acc =
      match String.Map.find acc name with
      | None -> String.Map.add acc name kind
      | Some kind' ->
        match kind, kind' with
        | Required, Required ->
          of_sexp_errorf sexp "library %S is present twice" name
        | (Optional|Forbidden), (Optional|Forbidden) ->
          acc
        | Optional, Required | Required, Optional ->
          of_sexp_errorf sexp
            "library %S is present both as an optional and required dependency"
            name
        | Forbidden, Required | Required, Forbidden ->
          of_sexp_errorf sexp
            "library %S is present both as a forbidden and required dependency"
            name
    in
    ignore (
      List.fold_left t ~init:String.Map.empty ~f:(fun acc x ->
        match x with
        | Lib_dep.Direct (_, s) -> add Required s acc
        | Select { choices; _ } ->
          List.fold_left choices ~init:acc ~f:(fun acc c ->
            let acc = String.Set.fold c.Lib_dep.required ~init:acc ~f:(add Optional) in
            String.Set.fold c.forbidden ~init:acc ~f:(add Forbidden)))
      : kind String.Map.t);
    t

  let of_pps pps =
    List.map pps ~f:(fun pp -> Lib_dep.of_pp (Loc.none, pp))
end

module Buildable = struct
  type t =
    { loc                      : Loc.t
    ; modules                  : Ordered_set_lang.t
    ; modules_without_implementation : Ordered_set_lang.t
    ; libraries                : Lib_dep.t list
    ; preprocess               : Preprocess_map.t
    ; preprocessor_deps        : Dep_conf.t list
    ; lint                     : Preprocess_map.t
    ; flags                    : Ordered_set_lang.Unexpanded.t
    ; ocamlc_flags             : Ordered_set_lang.Unexpanded.t
    ; ocamlopt_flags           : Ordered_set_lang.Unexpanded.t
    ; js_of_ocaml              : Js_of_ocaml.t
    ; allow_overlapping_dependencies : bool
    }

  let modules_field name =
    field name Ordered_set_lang.t ~default:Ordered_set_lang.standard

  let v1 =
    record_loc >>= fun loc ->
    field "preprocess" Preprocess_map.t ~default:Preprocess_map.default
    >>= fun preprocess ->
    field "preprocessor_deps" (list Dep_conf.t) ~default:[]
    >>= fun preprocessor_deps ->
    field "lint" Lint.t ~default:Lint.default
    >>= fun lint ->
    modules_field "modules"
    >>= fun modules ->
    modules_field "modules_without_implementation"
    >>= fun modules_without_implementation ->
    field "libraries" Lib_deps.t ~default:[]
    >>= fun libraries ->
    field_oslu "flags"          >>= fun flags          ->
    field_oslu "ocamlc_flags"   >>= fun ocamlc_flags   ->
    field_oslu "ocamlopt_flags" >>= fun ocamlopt_flags ->
    field "js_of_ocaml" (Js_of_ocaml.t) ~default:Js_of_ocaml.default
    >>= fun js_of_ocaml ->
    field_b "allow_overlapping_dependencies"
    >>= fun allow_overlapping_dependencies ->
    return
      { loc
      ; preprocess
      ; preprocessor_deps
      ; lint
      ; modules
      ; modules_without_implementation
      ; libraries
      ; flags
      ; ocamlc_flags
      ; ocamlopt_flags
      ; js_of_ocaml
      ; allow_overlapping_dependencies
      }

  let single_preprocess t =
    if Per_module.is_constant t.preprocess then
      Per_module.get t.preprocess (Module.Name.of_string "")
    else
      Preprocess.No_preprocessing
end

module Public_lib = struct
  type t =
    { name    : string
    ; package : Package.t
    ; sub_dir : string option
    }

  let public_name_field project =
    map_validate (field_o "public_name" string) ~f:(function
      | None -> Ok None
      | Some s ->
        match String.split s ~on:'.' with
        | [] -> assert false
        | pkg :: rest ->
          match Pkg.resolve project (Package.Name.of_string pkg) with
          | Ok pkg ->
            Ok (Some
                  { package = pkg
                  ; sub_dir =
                      if rest = [] then None else
                        Some (String.concat rest ~sep:"/")
                  ; name    = s
                  })
          | Error _ as e -> e)
end

module Sub_system_info = struct
  type t = ..
  type sub_system = t = ..

  type 'a parser =
    { short : (Loc.t -> 'a) option
    ; parse : 'a Sexp.Of_sexp.t
    }

  module type S = sig
    type t
    type sub_system += T of t
    val name    : Sub_system_name.t
    val loc     : t -> Loc.t
    val parsers : t parser Syntax.Versioned_parser.t
  end

  let all = Sub_system_name.Table.create ~default_value:None

  (* For parsing config files in the workspace *)
  let record_parser = ref return

  module Register(M : S) : sig end = struct
    open M

    let { short; parse } = snd (Syntax.Versioned_parser.last M.parsers)

    let short =
      match short with
      | None -> Short_syntax.Not_allowed
      | Some f -> Located f

    let () =
      match Sub_system_name.Table.get all name with
      | Some _ ->
        Exn.code_error "Sub_system_info.register: already registered"
          [ "name", Sexp.To_sexp.string (Sub_system_name.to_string name) ];
      | None ->
        Sub_system_name.Table.set all ~key:name ~data:(Some (module M : S));
        let p = !record_parser in
        let name_s = Sub_system_name.to_string name in
        record_parser := (fun acc ->
          field_o name_s ~short parse >>= function
          | None   -> p acc
          | Some x ->
            let acc = Sub_system_name.Map.add acc name (T x) in
            p acc)
  end

  let record_parser () = !record_parser Sub_system_name.Map.empty

  let get name = Option.value_exn (Sub_system_name.Table.get all name)
end

module Mode_conf = struct
  module T = struct
    type t =
      | Byte
      | Native
      | Best
    let compare (a : t) b = compare a b
  end
  include T

  let t =
    enum
      [ "byte"  , Byte
      ; "native", Native
      ; "best"  , Best
      ]

  module Set = struct
    include Set.Make(T)

    let t sexp = of_list (list t sexp)

    let default = of_list [Byte; Best]

    let eval t ~has_native =
      let best : Mode.t =
        if has_native then
          Native
        else
          Byte
      in
      let has_best = mem t Best in
      let byte = mem t Byte || (has_best && best = Byte) in
      let native = best = Native && (mem t Native || has_best) in
      { Mode.Dict.byte; native }
  end
end

module Library = struct
  module Kind = struct
    type t =
      | Normal
      | Ppx_deriver
      | Ppx_rewriter

    let t =
      enum
        [ "normal"       , Normal
        ; "ppx_deriver"  , Ppx_deriver
        ; "ppx_rewriter" , Ppx_rewriter
        ]
  end

  type t =
    { name                     : string
    ; public                   : Public_lib.t option
    ; synopsis                 : string option
    ; install_c_headers        : string list
    ; ppx_runtime_libraries    : (Loc.t * string) list
    ; modes                    : Mode_conf.Set.t
    ; kind                     : Kind.t
    ; c_flags                  : Ordered_set_lang.Unexpanded.t
    ; c_names                  : string list
    ; cxx_flags                : Ordered_set_lang.Unexpanded.t
    ; cxx_names                : string list
    ; library_flags            : Ordered_set_lang.Unexpanded.t
    ; c_library_flags          : Ordered_set_lang.Unexpanded.t
    ; self_build_stubs_archive : string option
    ; virtual_deps             : (Loc.t * string) list
    ; wrapped                  : bool
    ; optional                 : bool
    ; buildable                : Buildable.t
    ; dynlink                  : bool
    ; project_name             : Dune_project.Name.t
    ; sub_systems              : Sub_system_info.t Sub_system_name.Map.t
    }

  let v1 project =
    record
      (Buildable.v1 >>= fun buildable ->
       field      "name" library_name                                      >>= fun name                     ->
       Public_lib.public_name_field project                                >>= fun public                   ->
       field_o    "synopsis" string                                        >>= fun synopsis                 ->
       field      "install_c_headers" (list string) ~default:[]            >>= fun install_c_headers        ->
       field      "ppx_runtime_libraries" (list (located string)) ~default:[] >>= fun ppx_runtime_libraries    ->
       field_oslu "c_flags"                                                >>= fun c_flags                  ->
       field_oslu "cxx_flags"                                              >>= fun cxx_flags                ->
       field      "c_names" (list c_name) ~default:[]                      >>= fun c_names                  ->
       field      "cxx_names" (list cxx_name) ~default:[]                  >>= fun cxx_names                ->
       field_oslu "library_flags"                                          >>= fun library_flags            ->
       field_oslu "c_library_flags"                                        >>= fun c_library_flags          ->
       field      "virtual_deps" (list (located string)) ~default:[]       >>= fun virtual_deps             ->
       field      "modes" Mode_conf.Set.t ~default:Mode_conf.Set.default   >>= fun modes                    ->
       field      "kind" Kind.t ~default:Kind.Normal                       >>= fun kind                     ->
       field      "wrapped" bool ~default:true                             >>= fun wrapped                  ->
       field_b    "optional"                                               >>= fun optional                 ->
       field      "self_build_stubs_archive" (option string) ~default:None >>= fun self_build_stubs_archive ->
       field_b    "no_dynlink"                                             >>= fun no_dynlink               ->
       Sub_system_info.record_parser () >>= fun sub_systems ->
       field "ppx.driver" ignore ~default:() >>= fun () ->
       return
         { name
         ; public
         ; synopsis
         ; install_c_headers
         ; ppx_runtime_libraries
         ; modes
         ; kind
         ; c_names
         ; c_flags
         ; cxx_names
         ; cxx_flags
         ; library_flags
         ; c_library_flags
         ; self_build_stubs_archive
         ; virtual_deps
         ; wrapped
         ; optional
         ; buildable
         ; dynlink = not no_dynlink
         ; project_name = project.name
         ; sub_systems
         })

  let has_stubs t =
    match t.c_names, t.cxx_names, t.self_build_stubs_archive with
    | [], [], None -> false
    | _            -> true

  let stubs_archive t ~dir ~ext_lib =
    Path.relative dir (sprintf "lib%s_stubs%s" t.name ext_lib)

  let best_name t =
    match t.public with
    | None -> t.name
    | Some p -> p.name
end

module Install_conf = struct
  type file =
    { src : string
    ; dst : string option
    }

  let file sexp =
    match sexp with
    | Atom (_, A src) -> { src; dst = None }
    | List (_, [Atom (_, A src); Atom (_, A "as"); Atom (_, A dst)]) ->
      { src; dst = Some dst }
    | _ ->
      of_sexp_error sexp
        "invalid format, <name> or (<name> as <install-as>) expected"

  type t =
    { section : Install.Section.t
    ; files   : file list
    ; package : Package.t
    }

  let v1 project =
    record
      (field   "section" Install.Section.t >>= fun section ->
       field   "files"   (list file)       >>= fun files ->
       Pkg.field project                   >>= fun package ->
       return
         { section
         ; files
         ; package
         })
end

module Executables = struct

  module Link_mode = struct
    module T = struct
      type t =
        { mode : Mode_conf.t
        ; kind : Binary_kind.t
        }

      let compare a b =
        match compare a.mode b.mode with
        | Eq -> compare a.kind b.kind
        | ne -> ne
    end
    include T

    let make mode kind =
      { mode
      ; kind
      }

    let exe           = make Best Exe
    let object_       = make Best Object
    let shared_object = make Best Shared_object

    let byte_exe           = make Byte Exe

    let native_exe           = make Native Exe
    let native_object        = make Native Object
    let native_shared_object = make Native Shared_object

    let byte   = byte_exe
    let native = native_exe

    let simple =
      let open Sexp.Of_sexp in
      enum
        [ "exe"           , exe
        ; "object"        , object_
        ; "shared_object" , shared_object
        ; "byte"          , byte
        ; "native"        , native
        ]

    let t sexp =
      match sexp with
      | List _ ->
        let mode, kind = pair Mode_conf.t Binary_kind.t sexp in
        { mode; kind }
      | _ -> simple sexp

    module Set = struct
      include Set.Make(T)

      let t sexp : t =
        match list t sexp with
        | [] -> of_sexp_error sexp "No linking mode defined"
        | l ->
          let t = of_list l in
          if (mem t native_exe           && mem t exe          ) ||
             (mem t native_object        && mem t object_      ) ||
             (mem t native_shared_object && mem t shared_object) then
            of_sexp_error sexp
              "It is not allowed use both native and best \
               for the same binary kind."
          else
            t

      let default =
        of_list
          [ byte
          ; exe
          ]

      let best_install_mode t =
        if mem t exe then
          Some exe
        else if mem t native then
          Some native
        else if mem t byte then
          Some byte
        else
          None
    end
  end

  type t =
    { names            : (Loc.t * string) list
    ; link_executables : bool
    ; link_flags       : Ordered_set_lang.Unexpanded.t
    ; modes            : Link_mode.Set.t
    ; buildable        : Buildable.t
    }

  let common_v1 project names public_names ~multi =
    Buildable.v1 >>= fun buildable ->
    field      "link_executables"   bool ~default:true >>= fun link_executables ->
    field_oslu "link_flags"                            >>= fun link_flags ->
    field "modes" Link_mode.Set.t ~default:Link_mode.Set.default
    >>= fun modes ->
    map_validate
      (field "inline_tests" (fun _ -> true) ~default:false ~short:(This true))
      ~f:(function
        | false -> Ok ()
        | true  ->
          Error
            "Inline tests are only allowed in libraries.\n\
             See https://github.com/ocaml/dune/issues/745 for more details.")
    >>= fun () ->
    let t =
      { names
      ; link_executables
      ; link_flags
      ; modes
      ; buildable
      }
    in
    let to_install =
      match Link_mode.Set.best_install_mode t.modes with
      | None -> []
      | Some mode ->
        let ext =
          match mode.mode with
          | Native | Best -> ".exe"
          | Byte -> ".bc"
        in
        List.map2 names public_names
          ~f:(fun (_, name) pub ->
            match pub with
            | None -> None
            | Some pub -> Some ({ Install_conf.
                                  src = name ^ ext
                                ; dst = Some pub
                                }))
        |> List.filter_map ~f:(fun x -> x)
    in
    match to_install with
    | [] ->
      (field_o "package" Sexp.Ast.loc >>= function
       | None -> return (t, None)
       | Some loc ->
         Loc.warn loc
           "This field is useless without a (public_name%s ...) field."
           (if multi then "s" else "");
         return (t, None))
    | files ->
      Pkg.field project >>= fun package ->
      return (t, Some { Install_conf. section = Bin; files; package })

  let public_name sexp =
    match string sexp with
    | "-" -> None
    | s   -> Some s

  let v1_multi project =
    record
      (field "names" (list (located string)) >>= fun names ->
       map_validate (field_o "public_names" (list public_name)) ~f:(function
         | None -> Ok (List.map names ~f:(fun _ -> None))
         | Some public_names ->
           if List.length public_names = List.length names then
             Ok public_names
           else
             Error "The list of public names must be of the same \
                    length as the list of names")
       >>= fun public_names ->
       common_v1 project names public_names ~multi:true)

  let v1_single project =
    record
      (field   "name" (located string) >>= fun name ->
       field_o "public_name" string >>= fun public_name ->
       common_v1 project [name] [public_name] ~multi:false)
end

module Rule = struct
  module Targets = struct
    type t =
      | Static of string list (* List of files in the current directory *)
      | Infer
  end


  module Mode = struct
    type t =
      | Standard
      | Fallback
      | Promote
      | Promote_but_delete_on_clean
      | Not_a_rule_stanza
      | Ignore_source_files

    let t =
      enum
        [ "standard"           , Standard
        ; "fallback"           , Fallback
        ; "promote"            , Promote
        ; "promote-until-clean", Promote_but_delete_on_clean
        ]

    let field = field "mode" t ~default:Standard
  end

  type t =
    { targets  : Targets.t
    ; deps     : Dep_conf.t list
    ; action   : Loc.t * Action.Unexpanded.t
    ; mode     : Mode.t
    ; locks    : String_with_vars.t list
    ; loc      : Loc.t
    }

  let v1 sexp =
    let loc = Sexp.Ast.loc sexp in
    match sexp with
    | List (loc, (Atom _ :: _)) ->
      { targets  = Infer
      ; deps     = []
      ; action   = (loc, Action.Unexpanded.t sexp)
      ; mode     = Standard
      ; locks    = []
      ; loc      = loc
      }
    | _ ->
      record
        (field "targets" (list file_in_current_dir)    >>= fun targets ->
         field "deps"    (list Dep_conf.t) ~default:[] >>= fun deps ->
         field "action"  (located Action.Unexpanded.t) >>= fun action ->
         field "locks"   (list String_with_vars.t) ~default:[] >>= fun locks ->
         map_validate
           (field_b "fallback" >>= fun fallback ->
            field_o "mode" Mode.t >>= fun mode ->
            return (fallback, mode))
           ~f:(function
             | true, Some _ ->
               Error "Cannot use both (fallback) and (mode ...) at the \
                      same time.\n\
                      (fallback) is the same as (mode fallback), \
                      please use the latter in new code."
             | false, Some mode -> Ok mode
             | true, None -> Ok Fallback
             | false, None -> Ok Standard)
         >>= fun mode ->
         return { targets = Static targets
                ; deps
                ; action
                ; mode
                ; locks
                ; loc
                })
        sexp

  type lex_or_yacc =
    { modules : string list
    ; mode    : Mode.t
    }

  let ocamllex_v1 sexp =
    match sexp with
    | List (_, List (_, _) :: _) ->
      record
        (field "modules" (list string) >>= fun modules ->
         Mode.field >>= fun mode ->
         return { modules; mode })
        sexp
    | _ ->
      { modules = list string sexp
      ; mode    = Standard
      }

  let ocamlyacc_v1 = ocamllex_v1

  let ocamllex_to_rule loc { modules; mode } =
    let module S = String_with_vars in
    List.map modules ~f:(fun name ->
      let src = name ^ ".mll" in
      let dst = name ^ ".ml"  in
      { targets = Static [dst]
      ; deps    = [File (S.virt_text __POS__ src)]
      ; action  =
          (loc,
           Chdir
             (S.virt_var __POS__ "ROOT",
              Run (S.virt_text __POS__ "ocamllex",
                   [ S.virt_text __POS__ "-q"
                   ; S.virt_text __POS__ "-o"
                   ; S.virt_var __POS__ "@"
                   ; S.virt_var __POS__"<"
                   ])))
      ; mode
      ; locks = []
      ; loc
      })

  let ocamlyacc_to_rule loc { modules; mode } =
    let module S = String_with_vars in
    List.map modules ~f:(fun name ->
      let src = name ^ ".mly" in
      { targets = Static [name ^ ".ml"; name ^ ".mli"]
      ; deps    = [File (S.virt_text __POS__ src)]
      ; action  =
          (loc,
           Chdir
             (S.virt_var __POS__ "ROOT",
              Run (S.virt_text __POS__ "ocamlyacc",
                   [S.virt_var __POS__ "<"])))
      ; mode
      ; locks = []
      ; loc
      })
end

module Menhir = struct
  type t =
    { merge_into : string option
    ; flags      : Ordered_set_lang.Unexpanded.t
    ; modules    : string list
    ; mode       : Rule.Mode.t
    ; loc        :  Loc.t
    }

  let v1 =
    record
      (field_o "merge_into" string >>= fun merge_into ->
       field_oslu "flags" >>= fun flags ->
       field "modules" (list string) >>= fun modules ->
       Rule.Mode.field >>= fun mode ->
       return
         { merge_into
         ; flags
         ; modules
         ; mode
         ; loc = Loc.none
         }
      )
end

module Provides = struct
  type t =
    { name : string
    ; file : string
    }

(*  let v1 sexp =
    match sexp with
    | Atom (_, s) ->
      { name = s
      ; file =
          match String.lsplit2 s ~on:':' with
          | None        -> s
          | Some (_, s) -> s
      }
    | List (_, [Atom (_, s); List (_, [Atom (_, "file"); Atom (_, file)])]) ->
      { name = s
      ; file
      }
    | sexp ->
    of_sexp_error sexp "[<name>] or [<name> (file <file>)] expected"*)
end

module Alias_conf = struct
  type t =
    { name    : string
    ; deps    : Dep_conf.t list
    ; action  : (Loc.t * Action.Unexpanded.t) option
    ; locks   : String_with_vars.t list
    ; package : Package.t option
    }

  let alias_name sexp =
    let s = string sexp in
    if Filename.basename s <> s then
      of_sexp_errorf sexp "%S is not a valid alias name" s
    else
      s

  let v1 project =
    record
      (field "name" alias_name                          >>= fun name ->
       field "deps" (list Dep_conf.t) ~default:[]       >>= fun deps ->
       field_o "package" (Pkg.t project)                >>= fun package ->
       field_o "action" (located Action.Unexpanded.t)   >>= fun action ->
       field "locks" (list String_with_vars.t) ~default:[] >>= fun locks ->
       return
         { name
         ; deps
         ; action
         ; package
         ; locks
         })
end

module Copy_files = struct
  type t = { add_line_directive : bool
           ; glob : String_with_vars.t
           }

  let v1 = String_with_vars.t
end

module Documentation = struct
  type t =
    { package: Package.t
    ; mld_files: Ordered_set_lang.t
    }

  let v1 project =
    record
      (Pkg.field project >>= fun package ->
       field "mld_files" Ordered_set_lang.t ~default:Ordered_set_lang.standard
       >>= fun mld_files ->
       return
         { package
         ; mld_files
         }
      )
end

module Env = struct
  type config =
    { flags          : Ordered_set_lang.Unexpanded.t
    ; ocamlc_flags   : Ordered_set_lang.Unexpanded.t
    ; ocamlopt_flags : Ordered_set_lang.Unexpanded.t
    }

  type pattern =
    | Profile of string
    | Any

  type t =
    { loc   : Loc.t
    ; rules : (pattern * config) list
    }

  let config =
    record
      (field_oslu "flags"          >>= fun flags          ->
       field_oslu "ocamlc_flags"   >>= fun ocamlc_flags   ->
       field_oslu "ocamlopt_flags" >>= fun ocamlopt_flags ->
       return { flags; ocamlc_flags; ocamlopt_flags })

  let rule = function
    | List (loc, Atom (_, A pat) :: fields) ->
      let pat =
        match pat with
        | "_" -> Any
        | s   -> Profile s
      in
      (pat, config (List (loc, fields)))
    | sexp ->
      of_sexp_error sexp
        "S-expression of the form (<profile> <fields>) expected"
end

module Stanza = struct
  type t =
    | Library     of Library.t
    | Executables of Executables.t
    | Rule        of Rule.t
    | Provides    of Provides.t
    | Install     of Install_conf.t
    | Alias       of Alias_conf.t
    | Copy_files  of Copy_files.t
    | Menhir      of Menhir.t
    | Documentation of Documentation.t
    | Env         of Env.t
end

module Stanzas = struct
  type t = Stanza.t list

  type syntax = OCaml | Plain

  open Stanza

  let rules l = List.map l ~f:(fun x -> Rule x)

  let execs (exe, install) =
    match install with
    | None -> [Executables exe]
    | Some i -> [Executables exe; Install i]

  exception Include_loop of Path.t * (Loc.t * Path.t) list

  let rec v1 project ~file ~include_stack : Stanza.t list Sexp.Of_sexp.t =
    sum
      [ cstr "library"     (Library.v1 project @> nil) (fun x -> [Library x])
      ; cstr "executable"  (Executables.v1_single project @> nil) execs
      ; cstr "executables" (Executables.v1_multi  project @> nil) execs
      ; cstr_loc "rule"      (Rule.v1     @> nil) (fun loc x -> [Rule { x with loc }])
      ; cstr_loc "ocamllex" (Rule.ocamllex_v1 @> nil)
          (fun loc x -> rules (Rule.ocamllex_to_rule loc x))
      ; cstr_loc "ocamlyacc" (Rule.ocamlyacc_v1 @> nil)
          (fun loc x -> rules (Rule.ocamlyacc_to_rule loc x))
      ; cstr_loc "menhir" (Menhir.v1 @> nil)
          (fun loc x -> [Menhir { x with loc }])
      ; cstr "install"     (Install_conf.v1 project @> nil) (fun x -> [Install     x])
      ; cstr "alias"       (Alias_conf.v1 project @> nil)   (fun x -> [Alias       x])
      ; cstr "copy_files" (Copy_files.v1 @> nil)
          (fun glob -> [Copy_files {add_line_directive = false; glob}])
      ; cstr "copy_files#" (Copy_files.v1 @> nil)
          (fun glob -> [Copy_files {add_line_directive = true; glob}])
      ; cstr_rest_loc "env" nil Env.rule
          (fun loc rules -> [Env { loc; rules }])
      (* Just for validation and error messages *)
      ; cstr "jbuild_version" (Jbuild_version.t @> nil) (fun _ -> [])
      ; cstr_loc "include" (relative_file @> nil) (fun loc fn ->
          let include_stack = (loc, file) :: include_stack in
          let dir = Path.parent_exn file in
          let file = Path.relative dir fn in
          if not (Path.exists file) then
            Loc.fail loc "File %s doesn't exist."
              (Path.to_string_maybe_quoted file);
          if List.exists include_stack ~f:(fun (_, f) -> f = file) then
            raise (Include_loop (file, include_stack));
          let sexps = Io.Sexp.load file ~mode:Many in
          parse project sexps ~default_version:Jbuild_version.V1 ~file ~include_stack)
      ; cstr "documentation" (Documentation.v1 project @> nil)
          (fun d -> [Documentation d])
      ]

  and select
    :  Jbuild_version.t
    -> Dune_project.t
    -> file:Path.t
    -> include_stack:(Loc.t * Path.t) list
    -> Stanza.t list Sexp.Of_sexp.t = function
    | V1  -> v1

  and parse ~default_version ~file ~include_stack project sexps =
    let versions, sexps =
      List.partition_map sexps ~f:(function
        | List (loc, [Atom (_, A "jbuild_version"); ver]) ->
          Left (Jbuild_version.t ver, loc)
        | sexp -> Right sexp)
    in
    let version =
      match versions with
      | [] -> default_version
      | [(v, _)] -> v
      | _ :: (_, loc) :: _ ->
        Loc.fail loc "jbuild_version specified too many times"
    in
    let l =
      List.concat_map sexps ~f:(select version project ~file ~include_stack)
    in
    match List.filter_map l ~f:(function Env e -> Some e | _ -> None) with
    | _ :: e :: _ ->
      Loc.fail e.loc "The 'env' stanza cannot appear more than once"
    | _ -> l

  let parse ?(default_version=Jbuild_version.latest_stable) ~file project sexps =
    try
      parse project sexps ~default_version ~include_stack:[] ~file
    with
    | Include_loop (_, []) -> assert false
    | Include_loop (file, last :: rest) ->
      let loc = fst (Option.value (List.last rest) ~default:last) in
      let line_loc (loc, file) =
        sprintf "%s:%d"
          (Path.to_string_maybe_quoted file)
          loc.Loc.start.pos_lnum
      in
      Loc.fail loc
        "Recursive inclusion of jbuild files detected:\n\
         File %s is included from %s%s"
        (Path.to_string_maybe_quoted file)
        (line_loc last)
        (String.concat ~sep:""
           (List.map rest ~f:(fun x ->
              sprintf
                "\n--> included from %s"
                (line_loc x))))
end
