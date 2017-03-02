open Import
open Sexp.Of_sexp

(* This file defines the jbuild types as well as the S-expression syntax for the various
   supported version of the specification.

   [vN] is for the version [N] of the specification and [vjs] is for the rolling
   [jane_street] version. When they are all the same, sexp parsers are just named [t].
*)

module Jbuild_version = struct
  type t =
    | V1
    | Vjs

  let t =
    enum
      [ "1", V1
      ; "jane_street", Vjs
      ]

  let latest_stable = V1
end

let invalid_module_name sexp =
  of_sexp_error sexp "invalid module name"

let module_name sexp =
  match string sexp with
  | "" -> invalid_module_name sexp
  | s ->
    if s.[0] = '_' then invalid_module_name sexp;
    String.iter s ~f:(function
      | 'A'..'Z' | 'a'..'z' | '_' -> ()
      | _ -> invalid_module_name sexp);
    String.capitalize_ascii s

let module_names sexp = String_set.of_list (list module_name sexp)

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

module Raw_string () : sig
  type t = private string
  val to_string : t -> string
  val of_string : string -> t
  val t : t Sexp.Of_sexp.t
end = struct
  type t = string
  let to_string t = t
  let of_string t = t
  let t = string
end

module Raw_command = Raw_string ()

module Pp = struct
  include Raw_string ()

  let of_string s =
    assert (not (String.is_prefix s ~prefix:"-"));
    of_string s

  let t sexp =
    let s = string sexp in
    if String.is_prefix s ~prefix:"-" then
      of_sexp_error sexp "flag not allowed here"
    else
      of_string s

  let compare : t -> t -> int = Pervasives.compare
end

module Pp_set = Set.Make(Pp)

module Pp_or_flag = struct
  type t =
    | PP of Pp.t
    | Flag of string

  let of_string s =
    if String.is_prefix s ~prefix:"-" then
      Flag s
    else
      PP (Pp.of_string s)

  let t sexp = of_string (string sexp)

  let split l =
    List.partition_map l ~f:(function
      | PP pp  -> Inl pp
      | Flag s -> Inr s)
end

module Dep_conf = struct
  type t =
    | File of String_with_vars.t
    | Alias of String_with_vars.t
    | Glob_files of String_with_vars.t
    | Files_recursively_in of String_with_vars.t

  let t =
    let t =
      let cstr name f =
        cstr name (String_with_vars.t @> nil) f
      in
      sum
        [ cstr "file"                 (fun x -> File x)
        ; cstr "alias"                (fun x -> Alias x)
        ; cstr "glob_files"           (fun x -> Glob_files x)
        ; cstr "files_recursively_in" (fun x -> Files_recursively_in x)
        ]
    in
    fun sexp ->
      match sexp with
      | Atom _ -> File (String_with_vars.t sexp)
      | List _ -> t sexp

  open Sexp
  let sexp_of_t = function
    | File t ->
      List [Atom "file" ; String_with_vars.sexp_of_t t]
    | Alias t ->
      List [Atom "alias" ; String_with_vars.sexp_of_t t]
    | Glob_files t ->
      List [Atom "glob_files" ; String_with_vars.sexp_of_t t]
    | Files_recursively_in t ->
      List [Atom "files_recursively_in" ; String_with_vars.sexp_of_t t]
end

module Preprocess = struct
  type pps = { pps : Pp_set.t; flags : string list }
  type t =
    | No_preprocessing
    | Command of String_with_vars.t
    | Pps of pps

  let t =
    sum
      [ cstr "no_preprocessing" nil No_preprocessing
      ; cstr "command"          (String_with_vars.t @> nil) (fun x -> Command x)
      ; cstr "pps"              (list Pp_or_flag.t  @> nil) (fun l ->
          let pps, flags = Pp_or_flag.split l in
          Pps { pps = Pp_set.of_list pps; flags })
      ]

  let pp_set = function
    | Pps { pps; _ } -> pps
    | _ -> Pp_set.empty
end

module Preprocess_map = struct
  type t =
    | For_all  of Preprocess.t
    | Per_file of Preprocess.t String_map.t

  let find module_name t =
    match t with
    | For_all  pp  -> pp
    | Per_file map -> String_map.find_default module_name map ~default:No_preprocessing

  let default_v1 = For_all No_preprocessing
  let default_vjs = For_all (Pps { pps = Pp_set.singleton (Pp.of_string "ppx_jane"); flags = [] })

  let t sexp =
    match sexp with
    | List (_, Atom (_, "per_file") :: rest) -> begin
        List.concat_map rest ~f:(fun sexp ->
          let pp, names = pair Preprocess.t module_names sexp in
          List.map (String_set.elements names) ~f:(fun name -> (name, pp)))
        |> String_map.of_alist
        |> function
        | Ok map -> Per_file map
        | Error (name, _, _) ->
          of_sexp_error sexp (sprintf "module %s present in two different sets" name)
      end
    | sexp -> For_all (Preprocess.t sexp)

  let pps = function
    | For_all pp -> Preprocess.pp_set pp
    | Per_file map ->
      String_map.fold map ~init:Pp_set.empty ~f:(fun ~key:_ ~data:pp acc ->
        Pp_set.union acc (Preprocess.pp_set pp))
end

let field_osl name =
  field name Ordered_set_lang.t ~default:Ordered_set_lang.standard

let field_oslu name =
  field name Ordered_set_lang.Unexpanded.t ~default:Ordered_set_lang.Unexpanded.standard

module Js_of_ocaml = struct
  type t =
    { flags            : string list
    ; javascript_files : string list
    }


  let t =
    record
      (field "flags"            (list string) ~default:[] >>= fun flags ->
       field "javascript_files" (list string) ~default:[] >>= fun javascript_files ->
       return { flags; javascript_files })
end

module Lib_dep = struct
  type literal = Pos of string | Neg of string

  type choice =
    { lits : literal list
    ; file : string
    }

  type select = { result_fn : string; choices : choice list }

  type t =
    | Direct of string
    | Select of select

  let choice = function
    | List (_, l) as sexp ->
      let rec loop acc = function
        | [Atom (_, "->"); sexp] ->
          { lits = List.rev acc
          ; file = file sexp
          }
        | Atom (_, "->") :: _ | List _ :: _ | [] ->
          of_sexp_error sexp "(<[!]libraries>... -> <file>) expected"
        | Atom (_, s) :: l ->
          let len = String.length s in
          if len > 0 && s.[0] = '!' then
            let s = String.sub s ~pos:1 ~len:(len - 1) in
            loop (Neg s :: acc) l
          else
            loop (Pos s :: acc) l
      in
      loop [] l
    | sexp -> of_sexp_error sexp "(<library-name> <code>) expected"

  let sexp_of_choice { lits; file } : Sexp.t =
    List (List.fold_right lits ~init:[Atom "->"; Atom file]
       ~f:(fun lit acc ->
            match lit with
            | Pos s -> Sexp.Atom s :: acc
            | Neg s -> Sexp.Atom ("!" ^ s) :: acc))

  let t = function
    | Atom (_, s) ->
      Direct s
    | List (_, Atom (_, "select") :: m :: Atom (_, "from") :: libs) ->
      Select { result_fn = file m
             ; choices   = List.map libs ~f:choice
             }
    | sexp ->
      of_sexp_error sexp "<library> or (select <module> from <libraries...>) expected"

  let to_lib_names = function
    | Direct s -> [s]
    | Select s ->
      List.concat_map s.choices ~f:(fun x ->
        List.map x.lits ~f:(function
          | Pos x -> x
          | Neg x -> x))

  let direct s = Direct s
end

module Buildable = struct
  type t =
    { modules                  : Ordered_set_lang.t
    ; libraries                : Lib_dep.t list
    ; preprocess               : Preprocess_map.t
    ; preprocessor_deps        : Dep_conf.t list
    ; flags                    : Ordered_set_lang.t
    ; ocamlc_flags             : Ordered_set_lang.t
    ; ocamlopt_flags           : Ordered_set_lang.t
    }

  let common ~pp_default =
    field "preprocess" Preprocess_map.t ~default:pp_default
    >>= fun preprocess ->
    field "preprocessor_deps" (list Dep_conf.t) ~default:[]
    >>= fun preprocessor_deps        ->
    field "modules" (fun s -> Ordered_set_lang.(map (t s)) ~f:String.capitalize_ascii)
      ~default:Ordered_set_lang.standard
    >>= fun modules ->
    field "libraries" (list Lib_dep.t) ~default:[]
    >>= fun libraries ->
    field_osl "flags"          >>= fun flags          ->
    field_osl "ocamlc_flags"   >>= fun ocamlc_flags   ->
    field_osl "ocamlopt_flags" >>= fun ocamlopt_flags ->
    return
      { preprocess
      ; preprocessor_deps
      ; modules
      ; libraries
      ; flags
      ; ocamlc_flags
      ; ocamlopt_flags
      }

  let v1 = common ~pp_default:Preprocess_map.default_v1

  let vjs =
    common ~pp_default:Preprocess_map.default_vjs >>= fun t ->
    field "extra_disabled_warnings" (list int) ~default:[]
    >>= fun extra_disabled_warnings  ->
    let t =
      if Ordered_set_lang.is_standard t.flags && extra_disabled_warnings <> [] then
        let flags =
          Ordered_set_lang.append t.flags
            (Ordered_set_lang.t
               (List (Loc.none,
                      [ Atom (Loc.none, "-w")
                      ; Atom
                          (Loc.none,
                           String.concat ~sep:""
                             (List.map extra_disabled_warnings ~f:(sprintf "-%d")))
                      ])))
        in
        { t with flags }
      else
        t
    in
    return t
end

module Public_lib = struct
  type t =
    { name    : string (* Full public name *)
    ; package : string (* Package it is part of *)
    ; sub_dir : string option (* Subdirectory inside the installation directory *)
    }

  let of_public_name s =
    match String.split s ~on:'.' with
    | [] -> assert false
    | pkg :: rest ->
      { package = pkg
      ; sub_dir = if rest = [] then None else Some (String.concat rest ~sep:"/")
      ; name    = s
      }

  let t sexp = of_public_name (string sexp)
end

module Library = struct
  module Kind = struct
    type t =
      | Normal
      | Ppx_type_conv_plugin
      | Ppx_rewriter

    let t =
      enum
        [ "normal"               , Normal
        ; "ppx_type_conv_plugin" , Ppx_type_conv_plugin
        ; "ppx_rewriter"         , Ppx_rewriter
        ]
  end

  type t =
    { name                     : string
    ; public                   : Public_lib.t option
    ; synopsis                 : string option
    ; install_c_headers        : string list
    ; ppx_runtime_libraries    : string list
    ; modes                    : Mode.t list
    ; kind                     : Kind.t
    ; c_flags                  : Ordered_set_lang.Unexpanded.t
    ; c_names                  : string list
    ; cxx_flags                : Ordered_set_lang.Unexpanded.t
    ; cxx_names                : string list
    ; includes                 : String_with_vars.t list
    ; library_flags            : String_with_vars.t list
    ; c_library_flags          : Ordered_set_lang.Unexpanded.t
    ; self_build_stubs_archive : string option
    ; js_of_ocaml              : Js_of_ocaml.t option
    ; virtual_deps             : string list
    ; wrapped                  : bool
    ; optional                 : bool
    ; buildable                : Buildable.t
    }

  let v1 =
    record
      (Buildable.v1 >>= fun buildable ->
       field      "name" library_name                                      >>= fun name                     ->
       field_o    "public_name" Public_lib.t                               >>= fun public              ->
       field_o    "synopsis" string                                        >>= fun synopsis                 ->
       field      "install_c_headers" (list string) ~default:[]            >>= fun install_c_headers        ->
       field      "ppx_runtime_libraries" (list string) ~default:[]        >>= fun ppx_runtime_libraries    ->
       field_oslu "c_flags"                                                >>= fun c_flags                  ->
       field_oslu "cxx_flags"                                              >>= fun cxx_flags                ->
       field      "c_names" (list string) ~default:[]                      >>= fun c_names                  ->
       field      "cxx_names" (list string) ~default:[]                    >>= fun cxx_names                ->
       field      "library_flags" (list String_with_vars.t) ~default:[]    >>= fun library_flags            ->
       field_oslu "c_library_flags"                                        >>= fun c_library_flags          ->
       field_o    "js_of_ocaml" Js_of_ocaml.t                              >>= fun js_of_ocaml              ->
       field      "virtual_deps" (list string) ~default:[]                 >>= fun virtual_deps             ->
       field      "modes" (list Mode.t) ~default:Mode.all                  >>= fun modes                    ->
       field      "kind" Kind.t ~default:Kind.Normal                       >>= fun kind                     ->
       field      "wrapped" bool ~default:true                             >>= fun wrapped                  ->
       field_b    "optional"                                               >>= fun optional                 ->
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
         ; includes = []
         ; library_flags
         ; c_library_flags
         ; self_build_stubs_archive = None
         ; js_of_ocaml
         ; virtual_deps
         ; wrapped
         ; optional
         ; buildable
         })

  let vjs =
    record
      (ignore_fields ["inline_tests"; "skip_from_default"; "lint"] >>= fun () ->
       Buildable.vjs >>= fun buildable ->
       field      "name" library_name                                      >>= fun name                     ->
       field_o    "public_name" Public_lib.t                               >>= fun public              ->
       field_o    "synopsis" string                                        >>= fun synopsis                 ->
       field      "install_c_headers" (list string) ~default:[]            >>= fun install_c_headers        ->
       field      "ppx_runtime_libraries" (list string) ~default:[]        >>= fun ppx_runtime_libraries    ->
       field_oslu "c_flags"                                                >>= fun c_flags                  ->
       field_oslu "cxx_flags"                                              >>= fun cxx_flags                ->
       field      "c_names" (list string) ~default:[]                      >>= fun c_names                  ->
       field      "cxx_names" (list string) ~default:[]                    >>= fun cxx_names                ->
       field      "library_flags" (list String_with_vars.t) ~default:[]    >>= fun library_flags            ->
       field      "c_libraries" (list string) ~default:[]                  >>= fun c_libraries              ->
       field_oslu "c_library_flags"                                        >>= fun c_library_flags          ->
       field      "self_build_stubs_archive" (option string) ~default:None >>= fun self_build_stubs_archive ->
       field_o    "js_of_ocaml" Js_of_ocaml.t                              >>= fun js_of_ocaml              ->
       field      "virtual_deps" (list string) ~default:[]                 >>= fun virtual_deps             ->
       field      "modes" (list Mode.t) ~default:Mode.all                  >>= fun modes                    ->
       field      "includes" (list String_with_vars.t) ~default:[]         >>= fun includes                 ->
       field      "kind" Kind.t ~default:Kind.Normal                       >>= fun kind                     ->
       field      "wrapped" bool ~default:true                             >>= fun wrapped                  ->
       field_b    "optional"                                               >>= fun optional                 ->
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
         ; includes
         ; library_flags
         ; c_library_flags =
             Ordered_set_lang.Unexpanded.append
               (Ordered_set_lang.Unexpanded.t
                  (List (Loc.none,
                         List.map c_libraries ~f:(fun lib ->
                             Atom (Loc.none, "-l" ^ lib)))))
               c_library_flags
         ; self_build_stubs_archive
         ; js_of_ocaml
         ; virtual_deps
         ; wrapped
         ; optional
         ; buildable
         })

  let has_stubs t =
    match t.c_names, t.cxx_names, t.self_build_stubs_archive with
    | [], [], None -> false
    | _            -> true

  let stubs_archive t ~dir ~ext_lib =
    Path.relative dir (sprintf "lib%s_stubs%s" t.name ext_lib)

  let all_lib_deps t =
    List.map t.virtual_deps ~f:(fun s -> Lib_dep.Direct s) @ t.buildable.libraries
end

module Executables = struct
  type t =
    { names            : string list
    ; link_executables : bool
    ; link_flags       : string list
    ; buildable        : Buildable.t
    }

  let v1 =
    record
      (Buildable.v1 >>= fun buildable ->
       field   "names"              (list string)      >>= fun names ->
       field   "link_executables"   bool ~default:true >>= fun link_executables ->
       field   "link_flags"         (list string) ~default:[] >>= fun link_flags ->
       return
         { names
         ; link_executables
         ; link_flags
         ; buildable
         })

  let vjs =
    record
      (ignore_fields
         ["js_of_ocaml"; "only_shared_object"; "review_help"; "skip_from_default"]
       >>= fun () ->
       Buildable.vjs >>= fun buildable ->
       field   "names"              (list string)      >>= fun names ->
       field   "link_executables"   bool ~default:true >>= fun link_executables ->
       field   "link_flags"         (list string) ~default:[] >>= fun link_flags ->
       return
         { names
         ; link_executables
         ; link_flags
         ; buildable
         })
end

module Rule = struct
  type t =
    { targets : string list (** List of files in the current directory *)
    ; deps    : Dep_conf.t list
    ; action  : Action.Unexpanded.t
    }

  let common =
    field "targets" (list file_in_current_dir)    >>= fun targets ->
    field "deps"    (list Dep_conf.t) ~default:[] >>= fun deps ->
    field "action"  Action.Unexpanded.t           >>= fun action ->
    return { targets; deps; action }

  let v1 = record common

  let vjs =
    record
      (ignore_fields ["sandbox"] >>= fun () ->
       common)

  let ocamllex_v1 names =
    let str s = String_with_vars.of_string s in
    List.map names ~f:(fun name ->
      let src = name ^ ".mll" in
      let dst = name ^ ".ml"  in
      { targets = [dst]
      ; deps    = [File (str src)]
      ; action  =
          Shexp
            (Chdir
               (str "${ROOT}",
                Run (str "${bin:ocamllex}",
                     [str "-q"; str "-o"; str "${@}"; str "${<}"])))
      })

  let ocamllex_vjs = ocamllex_v1

  let ocamlyacc_v1 names =
    let str s = String_with_vars.of_string s in
    List.map names ~f:(fun name ->
      let src = name ^ ".mly" in
      { targets = [name ^ ".ml"; name ^ ".mli"]
      ; deps    = [File (str src)]
      ; action  =
          Shexp
            (Chdir
               (str "${ROOT}",
                Run (str "${bin:ocamlyacc}",
                     [str "${<}"])))
      })

  let ocamlyacc_vjs = ocamlyacc_v1
end

module Provides = struct
  type t =
    { name : string
    ; file : string
    }

  let v1 sexp =
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
      of_sexp_error sexp "[<name>] or [<name> (file <file>)] expected"

  let vjs = v1
end

module Install_conf = struct
  type file =
    { src : string
    ; dst : string option
    }

  let file sexp =
    match sexp with
    | Atom (_, src) -> { src; dst = None }
    | List (_, [Atom (_, src); Atom (_, "as"); Atom (_, dst)]) ->
      { src; dst = Some dst }
    | _ ->
      of_sexp_error sexp
        "invalid format, <name> or (<name> as <install-as>) expected"

  type t =
    { section : Install.Section.t
    ; files   : file list
    ; package : string option
    }

  let v1 =
    record
      (field   "section" Install.Section.t >>= fun section ->
       field   "files"   (list file)       >>= fun files ->
       field_o "package" string            >>= fun package ->
       return
         { section
         ; files
         ; package
         })

  let vjs = v1
end

module Alias_conf = struct
  type t =
    { name  : string
    ; deps  : Dep_conf.t list
    ; action : Action.Unexpanded.t option
    }

  let common =
    field "name" string                        >>= fun name ->
    field "deps" (list Dep_conf.t) ~default:[] >>= fun deps ->
    field_o "action" Action.Unexpanded.t  >>= fun action ->
    return
      { name
      ; deps
      ; action
      }

  let v1 = record common

  let vjs =
    record
      (ignore_fields ["sandbox"] >>= fun () ->
       common)
end

module Stanza = struct
  type t =
    | Library     of Library.t
    | Executables of Executables.t
    | Rule        of Rule.t
    | Provides    of Provides.t
    | Install     of Install_conf.t
    | Alias       of Alias_conf.t

  let rules l = List.map l ~f:(fun x -> Rule x)

  let v1 =
    sum
      [ cstr "library"     (Library.v1 @> nil)      (fun x -> [Library     x])
      ; cstr "executables" (Executables.v1 @> nil)  (fun x -> [Executables x])
      ; cstr "rule"        (Rule.v1 @> nil)         (fun x -> [Rule        x])
      ; cstr "ocamllex"    (list string @> nil)     (fun x -> rules (Rule.ocamllex_v1 x))
      ; cstr "ocamlyacc"   (list string @> nil)     (fun x -> rules (Rule.ocamlyacc_v1 x))
      ; cstr "install"     (Install_conf.v1 @> nil) (fun x -> [Install     x])
      ; cstr "alias"       (Alias_conf.v1 @> nil)   (fun x -> [Alias       x])
      (* Just for validation and error messages *)
      ; cstr "jbuild_version" (Jbuild_version.t @> nil) (fun _ -> [])
      ]

  let vjs =
    let ign name = cstr name ((fun _ -> ()) @> nil) (fun () -> []) in
    sum
      [ cstr "library"     (Library.vjs @> nil)      (fun x -> [Library     x])
      ; cstr "executables" (Executables.vjs @> nil)  (fun x -> [Executables x])
      ; cstr "rule"        (Rule.vjs @> nil)         (fun x -> [Rule        x])
      ; cstr "ocamllex"    (list string @> nil)      (fun x -> rules (Rule.ocamllex_vjs x))
      ; cstr "ocamlyacc"   (list string @> nil)      (fun x -> rules (Rule.ocamlyacc_vjs x))
      ; cstr "provides"    (Provides.vjs @> nil)     (fun x -> [Provides    x])
      ; cstr "install"     (Install_conf.vjs @> nil) (fun x -> [Install     x])
      ; cstr "alias"       (Alias_conf.vjs @> nil)   (fun x -> [Alias       x])
      ; ign "enforce_style"
      ; ign "toplevel_expect_tests"
      ; ign "unified_tests"
      ; ign "embed"
      (* Just for validation and error messages  *)
      ; cstr "jbuild_version" (Jbuild_version.t @> nil) (fun _ -> [])
      ]

  let select : Jbuild_version.t -> t list Sexp.Of_sexp.t = function
    | V1  -> v1
    | Vjs -> vjs

  let lib_names ts =
    List.fold_left ts ~init:String_set.empty ~f:(fun acc (_, stanzas) ->
      List.fold_left stanzas ~init:acc ~f:(fun acc -> function
        | Library lib ->
          String_set.add lib.name
            (match lib.public with
             | None -> acc
             | Some { name; _ } -> String_set.add name acc)
        | _ -> acc))
end

module Stanzas = struct
  type t = Stanza.t list

  let resolve_packages ts ~dir
        ~(visible_packages : Package.t String_map.t)
        ~(closest_packages : Package.t list) =
    let error fmt =
      Loc.fail (Loc.in_file (Path.to_string (Path.relative dir "jbuild"))) fmt
    in
    let package_listing packages =
      let longest_pkg = List.longest_map packages ~f:(fun p -> p.Package.name) in
      String.concat ~sep:"\n"
        (List.map packages ~f:(fun pkg ->
           sprintf "- %-*s (because of %s)" longest_pkg pkg.Package.name
             (Path.to_string (Path.relative pkg.path (pkg.name ^ ".opam")))))
    in
    let check pkg =
      if not (String_map.mem pkg visible_packages) then
        error "package %S is not visible here.\n\
               The only packages I know of in %S are:\n\
               %s%s"
          pkg
          (Path.to_string dir)
          (package_listing (String_map.values visible_packages))
          (hint pkg (String_map.keys visible_packages))
    in
    let default () =
      match closest_packages with
      | [pkg] -> pkg
      | [] -> error "no packages are defined here"
      | _ :: _ :: _ ->
        error "I can't determine automatically which package your (install ...) \
               stanzas are for in this directory. I have the choice between these ones:\n\
               %s\n\
               You need to add a (package ...) field in your (install ...) stanzas"
          (package_listing closest_packages)
    in
    List.map ts ~f:(fun (stanza : Stanza.t) ->
      match stanza with
      | Library { public = Some { package; _ }; _ } ->
        check package;
        stanza
      | Install { package = Some pkg; _ } ->
        check pkg;
        stanza
      | Install ({ package = None; _ } as install) ->
        Install { install with package = Some (default ()).name }
      | _ -> stanza)

  let parse sexps ~dir ~visible_packages =
    let versions, sexps =
      List.partition_map sexps ~f:(function
          | List (loc, [Atom (_, "jbuild_version"); ver]) ->
            Inl (Jbuild_version.t ver, loc)
          | sexp -> Inr sexp)
    in
    let version =
      match versions with
      | [] -> Jbuild_version.latest_stable
      | [(v, _)] -> v
      | _ :: (_, loc) :: _ ->
        Loc.fail loc "jbuild_version specified too many times"
    in
    List.concat_map sexps ~f:(Stanza.select version)
    |> resolve_packages ~dir ~visible_packages
end
