open Import
open Sexp.Of_sexp

type sexp = Sexp.t = Atom of string | List of sexp list
let of_sexp_error = Sexp.of_sexp_error

let invalid_module_name sexp =
  of_sexp_error "invalid module name" sexp

let module_name sexp =
  match string sexp with
  | "" -> invalid_module_name sexp
  | s ->
    if s.[0] = '_' then invalid_module_name sexp;
    String.iter s ~f:(function
      | 'A'..'Z' | 'a'..'z' | '_' -> ()
      | _ -> invalid_module_name sexp);
    String.capitalize s

let module_names sexp = String_set.of_list (list module_name sexp)

let invalid_lib_name sexp =
  of_sexp_error "invalid library name" sexp

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
    Sexp.of_sexp_error "'.' and '..' are not valid filenames" sexp
  | fn -> fn

let file_in_current_dir sexp =
  match string sexp with
  | "." | ".." ->
    Sexp.of_sexp_error "'.' and '..' are not valid filenames" sexp
  | fn ->
    if Filename.dirname fn <> Filename.current_dir_name then
      Sexp.of_sexp_error "file in current directory expected" sexp;
    fn

module Raw_string () : sig
  type t = private string
  val to_string : t -> string
  val of_string : string -> t
  val t : Sexp.t -> t
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
    let s =
      match s with
      (* For compatibility with the old hardcoded ppx sets of Jane Street jenga rules *)
      | "BASE"        -> "ppx_base"
      | "JANE"        -> "ppx_jane"
      | "JANE_KERNEL" -> "ppx_jane_kernel"
      | s -> s
    in
    of_string s

  let t sexp =
    let s = string sexp in
    if String.is_prefix s ~prefix:"-" then
      of_sexp_error "flag not allowed here" sexp
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

module User_action = struct
  module Mini_shexp = struct
    type 'a t =
      | Run    of 'a * 'a list
      | Chdir  of 'a * 'a t
      | Setenv of 'a * 'a * 'a t

    let rec t a sexp =
      match sexp with
      | List (Atom "run" :: prog :: args) -> Run (a prog, List.map args ~f:a)
      | List [ Atom "chdir"; dir; arg ] -> Chdir (a dir, t a arg)
      | List [ Atom "setenv"; var; value; arg ] -> Setenv (a var, a value, t a arg)
      | _ ->
        of_sexp_error "\
invalid action, expected one of:

  (run <prog> <args)
  (chdir <dir> <action>)
  (setenv <var> <value> <action>)
" sexp

    let rec map t ~f =
      match t with
      | Run (prog, args) -> Run (f prog, List.map args ~f)
      | Chdir (fn, t) -> Chdir (f fn, map t ~f)
      | Setenv (var, value, t) -> Setenv (f var, f value, map t ~f)

    let rec fold t ~init:acc ~f =
      match t with
      | Run (prog, args) -> List.fold_left args ~init:(f acc prog) ~f
      | Chdir (fn, t) -> fold t ~init:(f acc fn) ~f
      | Setenv (var, value, t) -> fold t ~init:(f (f acc var) value) ~f

    let to_action ~dir ~env (t : string t) =
      let rec loop vars dir = function
        | Chdir (fn, t) ->
          loop vars (Path.relative dir fn) t
        | Setenv (var, value, t) ->
          loop (String_map.add vars ~key:var ~data:value) dir t
        | Run (prog, args) ->
          { Action.
            prog = Path.relative dir prog
          ; args = args
          ; dir
          ; env = Context.extend_env ~vars ~env
          }
      in
      loop String_map.empty dir t
  end

  module T = struct
    type 'a t =
      | Bash of 'a
      | Shexp of 'a Mini_shexp.t

    let t a sexp =
      match sexp with
      | Atom _ -> Bash  (a              sexp)
      | List _ -> Shexp (Mini_shexp.t a sexp)

    let map t ~f =
      match t with
      | Bash x -> Bash (f x)
      | Shexp x -> Shexp (Mini_shexp.map x ~f)

    let fold t ~init ~f =
      match t with
      | Bash x -> f init x
      | Shexp x -> Mini_shexp.fold x ~init ~f
  end

  include T

  module Unexpanded = String_with_vars.Lift(T)

  let to_action ~dir ~env = function
    | Shexp shexp -> Mini_shexp.to_action ~dir ~env shexp
    | Bash cmd ->
      { Action.
        prog = Path.absolute "/bin/bash"
      ; args = ["-e"; "-u"; "-o"; "pipefail"; "-c"; cmd]
      ; env
      ; dir
      }
end

module Dep_conf = struct
  type t =
    | File of String_with_vars.t
    | Alias of String_with_vars.t
    | Glob_files of String_with_vars.t
    | Files_recursively_in of String_with_vars.t

  let t =
    let t =
      sum
        [ cstr "file"                 [String_with_vars.t] (fun x -> File x)
        ; cstr "alias"                [String_with_vars.t] (fun x -> Alias x)
        ; cstr "glob_files"           [String_with_vars.t] (fun x -> Glob_files x)
        ; cstr "files_recursively_in" [String_with_vars.t] (fun x -> Files_recursively_in x)
        ]
    in
    fun sexp ->
      match sexp with
      | Atom _ -> File (String_with_vars.t sexp)
      | List _ -> t sexp
end

module Preprocess = struct
  type t =
    | No_preprocessing
    | Command of String_with_vars.t
    | Metaquot
    | Pps of { pps : Pp_set.t; flags : string list }

  let t =
    sum
      [ cstr "no_preprocessing" [] No_preprocessing
      ; cstr "metaquot"         [] Metaquot
      ; cstr "command"          [String_with_vars.t] (fun x -> Command x)
      ; cstr "pps"              [list Pp_or_flag.t] (fun l ->
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

  let default = For_all (Pps { pps = Pp_set.singleton (Pp.of_string "JANE"); flags = [] })

  let t sexp =
    match sexp with
    | List (Atom "per_file" :: rest) -> begin
        List.concat_map rest ~f:(fun sexp ->
          let pp, names = pair Preprocess.t module_names sexp in
          List.map (String_set.elements names) ~f:(fun name -> (name, pp)))
        |> String_map.of_alist
        |> function
        | Ok map -> Per_file map
        | Error (name, _, _) ->
          Sexp.of_sexp_error (sprintf "module %s present in two different sets" name) sexp
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

let field_modules =
  field "modules" (fun s -> Ordered_set_lang.(map (t s)) ~f:String.capitalize_ascii)
    ~default:Ordered_set_lang.standard

let field_oslu name =
  field name Ordered_set_lang.Unexpanded.t ~default:Ordered_set_lang.Unexpanded.standard

let field_pp name =
  field name Preprocess_map.t ~default:Preprocess_map.default

module Library = struct
  type t =
    { name                  : string
    ; public_name           : string option
    ; libraries             : string list
    ; ppx_runtime_libraries : string list
    ; modules               : Ordered_set_lang.t
    ; c_flags               : Ordered_set_lang.Unexpanded.t
    ; c_names               : string list
    ; cxx_flags             : Ordered_set_lang.Unexpanded.t
    ; cxx_names             : string list
    ; library_flags         : Ordered_set_lang.Unexpanded.t
    ; cclibs                : Ordered_set_lang.Unexpanded.t
    ; preprocess            : Preprocess_map.t
    ; preprocessor_deps     : Dep_conf.t list
    ; self_build_stubs_archive : string option;
    }

  let t =
    record
      ~ignore:["js_of_ocaml"; "inline_tests"; "public_release"; "skip_from_default";
               "extra_disabled_warnings"; "lint"; "includes"; "flags"]
      [ field      "name"                  library_name
      ; field_o    "public_name"           string
      ; field      "libraries"             (list string) ~default:[]
      ; field      "ppx_runtime_libraries" (list string) ~default:[]
      ; field_modules
      ; field_oslu "c_flags"
      ; field_oslu "cxx_flags"
      ; field      "c_names"               (list string) ~default:[]
      ; field      "cxx_names"             (list string) ~default:[]
      ; field_oslu "library_flags"
      ; field_oslu "cclibs"
      ; field_pp   "preprocess"
      ; field      "preprocessor_deps"     (list Dep_conf.t) ~default:[]
      ; field      "self_build_stubs_archive" (option string) ~default:None
      ]
      (fun name public_name libraries ppx_runtime_libraries modules c_flags cxx_flags
        c_names cxx_names library_flags cclibs preprocess preprocessor_deps
        self_build_stubs_archive ->
        { name
        ; public_name
        ; libraries
        ; ppx_runtime_libraries
        ; modules
        ; c_names
        ; c_flags
        ; cxx_names
        ; cxx_flags
        ; library_flags
        ; cclibs
        ; preprocess
        ; preprocessor_deps
        ; self_build_stubs_archive
        })
end

module Executables = struct
  type t =
    { names            : string list
    ; object_public_name : string option
    ; link_executables : bool
    ; libraries        : string list
    ; link_flags       : string list
    ; modules          : Ordered_set_lang.t
    ; preprocess       : Preprocess_map.t
    }

  let t =
    record
      ~ignore:["js_of_ocaml"; "only_shared_object"; "review_help"; "skip_from_default"]
      [ field     "names"            (list string)
      ; field_o   "object_public_name" string
      ; field     "link_executables" bool ~default:true
      ; field     "libraries"        (list string) ~default:[]
      ; field     "link_flags"       (list string) ~default:[]
      ; field_modules
      ; field_pp  "preprocess"
      ]
      (fun names object_public_name link_executables libraries link_flags modules
        preprocess ->
         { names
         ; object_public_name
         ; link_executables
         ; libraries
         ; link_flags
         ; modules
         ; preprocess
         })
end

module Rule = struct
  type t =
    { targets : string list (** List of files in the current directory *)
    ; deps    : Dep_conf.t list
    ; action  : User_action.Unexpanded.t
    }

  let t =
    record
      [ field "targets" (list file_in_current_dir)
      ; field "deps"    (list Dep_conf.t)
      ; field "action"  User_action.Unexpanded.t
      ]
      (fun targets deps action ->
         { targets; deps; action })
end

module Ocamllex = struct
  type t = { names : string list }

  let t sexp = { names = list string sexp }
end

module Ocamlyacc = struct
  type t = { names : string list }

  let t sexp = { names = list string sexp }
end

module Provides = struct
  type t =
    { name : string
    ; file : string
    }

  let t sexp =
    match sexp with
    | Atom s ->
      { name = s
      ; file =
          match String.lsplit2 s ~on:':' with
          | None        -> s
          | Some (_, s) -> s
      }
    | List [Atom s; List [Atom "file"; Atom file]] ->
      { name = s
      ; file
      }
    | sexp ->
      of_sexp_error "[<name>] or [<name> (file <file>)] expected" sexp
end

module Stanza = struct
  type t =
    | Library     of Library.t
    | Executables of Executables.t
    | Rule        of Rule.t
    | Ocamllex    of Ocamllex.t
    | Ocamlyacc   of Ocamlyacc.t
    | Provides    of Provides.t
    | Other

  let t =
    sum
      [ cstr "library"     [Library.t]     (fun x -> Library     x)
      ; cstr "executables" [Executables.t] (fun x -> Executables x)
      ; cstr "rule"        [Rule.t]        (fun x -> Rule        x)
      ; cstr "ocamllex"    [Ocamllex.t]    (fun x -> Ocamllex    x)
      ; cstr "ocamlyacc"   [Ocamlyacc.t]   (fun x -> Ocamlyacc   x)
      ; cstr "provides"    [Provides.t]    (fun x -> Provides    x)
      ; cstr "alias"       [fun _ -> ()]   (fun _ -> Other        )
      ; cstr "enforce_style" [fun _ -> ()]   (fun _ -> Other        )
      ; cstr "toplevel_expect_tests" [fun _ -> ()] (fun _ -> Other)
      ; cstr "install" [fun _ -> ()] (fun _ -> Other)
      ; cstr "unified_tests" [fun _ -> ()] (fun _ -> Other)
      ; cstr "embed" [fun _ -> ()] (fun _ -> Other)
      ]

  let lib_names ts =
    List.fold_left ts ~init:String_set.empty ~f:(fun acc (_, stanzas) ->
      List.fold_left stanzas ~init:acc ~f:(fun acc -> function
        | Library lib ->
          String_set.add lib.name
            (match lib.public_name with
             | None -> acc
             | Some n -> String_set.add n acc)
        | _ -> acc))
end
