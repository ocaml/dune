open Import
open Decoder

let foreign_flags ~since =
  let check = Option.map since ~f:(fun since -> Syntax.since Stanza.syntax since) in
  let+ c = Ordered_set_lang.Unexpanded.field "c_flags" ?check
  and+ cxx = Ordered_set_lang.Unexpanded.field "cxx_flags" ?check in
  Foreign_language.Dict.make ~c ~cxx
;;

let menhir_flags ~since ~deleted_in =
  let decode =
    Syntax.since Menhir.syntax since
    >>> Syntax.deleted_in
          ~extra_info:"Use (menhir (flags ...)) instead."
          Menhir.syntax
          deleted_in
    >>> Ordered_set_lang.Unexpanded.decode
  in
  field_o "menhir_flags" decode
;;

module Inline_tests = struct
  type t =
    | Enabled
    | Disabled
    | Ignored

  let equal x y =
    match x, y with
    | Enabled, Enabled -> true
    | Disabled, Disabled -> true
    | Ignored, Ignored -> true
    | _, _ -> false
  ;;

  let decode = enum [ "enabled", Enabled; "disabled", Disabled; "ignored", Ignored ]

  let to_string = function
    | Enabled -> "enabled"
    | Disabled -> "disabled"
    | Ignored -> "ignored"
  ;;
end

module Odoc = struct
  type warnings =
    | Fatal
    | Nonfatal

  type sidebar =
    | Global
    | Per_package

  type support =
    | Root
    | Per_package

  type t =
    { warnings : warnings option
    ; sidebar : sidebar option
    ; support : support option
    }

  let empty = { warnings = None; sidebar = None; support = None }

  let warnings_equal x y =
    match x, y with
    | Fatal, Fatal | Nonfatal, Nonfatal -> true
    | (Fatal | Nonfatal), _ -> false
  ;;

  let sidebar_equal x y =
    match x, y with
    | Global, Global | Per_package, Per_package -> true
    | (Global | Per_package), _ -> false
  ;;

  let support_equal x y =
    match x, y with
    | Root, Root | Per_package, Per_package -> true
    | (Root | Per_package), _ -> false
  ;;

  let equal x y =
    Option.equal warnings_equal x.warnings y.warnings
    && Option.equal sidebar_equal x.sidebar y.sidebar
    && Option.equal support_equal x.support y.support
  ;;

  let warnings_decode = enum [ "fatal", Fatal; "nonfatal", Nonfatal ]
  let sidebar_decode = enum [ "global", Global; "per-package", Per_package ]
  let support_decode = enum [ "root", Root; "per-package", Per_package ]

  let decode =
    fields
    @@ let+ warnings = field_o "warnings" warnings_decode
       and+ sidebar = field_o "sidebar" sidebar_decode
       and+ support = field_o "support" support_decode in
       { warnings; sidebar; support }
  ;;
end

type config =
  { flags : Ocaml_flags.Spec.t
  ; foreign_flags : Ordered_set_lang.Unexpanded.t Foreign_language.Dict.t
  ; link_flags : Link_flags.Spec.t
  ; env_vars : Env.t
  ; binaries : File_binding.Unexpanded.t list option
  ; inline_tests : Inline_tests.t option
  ; menhir : Ordered_set_lang.Unexpanded.t Menhir_env.t
  ; odoc : Odoc.t
  ; js_of_ocaml : Ordered_set_lang.Unexpanded.t Js_of_ocaml.Env.t
  ; wasm_of_ocaml : Ordered_set_lang.Unexpanded.t Js_of_ocaml.Env.t
  ; coq : Coq_env.t
  ; rocq : Rocq_env.t
  ; format_config : Format_config.t option
  ; error_on_use : User_message.t option
  ; warn_on_load : User_message.t option
  ; bin_annot : bool option
  }

let dyn_of_config { bin_annot; _ } =
  let open Dyn in
  record [ "bin_annot", option bool bin_annot ]
;;

let equal_config
      { flags
      ; foreign_flags
      ; link_flags
      ; env_vars
      ; binaries
      ; inline_tests
      ; menhir
      ; odoc
      ; js_of_ocaml
      ; wasm_of_ocaml
      ; coq
      ; rocq
      ; format_config
      ; error_on_use
      ; warn_on_load
      ; bin_annot
      }
      t
  =
  Ocaml_flags.Spec.equal flags t.flags
  && Foreign_language.Dict.equal
       Ordered_set_lang.Unexpanded.equal
       foreign_flags
       t.foreign_flags
  && Link_flags.Spec.equal link_flags t.link_flags
  && Env.equal env_vars t.env_vars
  && Option.equal (List.equal File_binding.Unexpanded.equal) binaries t.binaries
  && Option.equal Inline_tests.equal inline_tests t.inline_tests
  && Menhir_env.equal menhir t.menhir
  && Odoc.equal odoc t.odoc
  && Coq_env.equal coq t.coq
  && Rocq_env.equal rocq t.rocq
  && Option.equal Format_config.equal format_config t.format_config
  && Js_of_ocaml.Env.equal js_of_ocaml t.js_of_ocaml
  && Js_of_ocaml.Env.equal wasm_of_ocaml t.wasm_of_ocaml
  && Option.equal User_message.equal error_on_use t.error_on_use
  && Option.equal User_message.equal warn_on_load t.warn_on_load
  && Option.equal Bool.equal bin_annot t.bin_annot
;;

let hash_config = Poly.hash

let empty_config =
  { flags = Ocaml_flags.Spec.standard
  ; foreign_flags = Foreign_language.Dict.make_both Ordered_set_lang.Unexpanded.standard
  ; link_flags = Link_flags.Spec.standard
  ; env_vars = Env.empty
  ; binaries = None
  ; inline_tests = None
  ; menhir = Menhir_env.empty
  ; odoc = Odoc.empty
  ; js_of_ocaml = Js_of_ocaml.Env.empty
  ; wasm_of_ocaml = Js_of_ocaml.Env.empty
  ; coq = Coq_env.default
  ; rocq = Rocq_env.default
  ; format_config = None
  ; error_on_use = None
  ; warn_on_load = None
  ; bin_annot = None
  }
;;

type pattern =
  | Profile of Profile.t
  | Any

let dyn_of_pattern =
  let open Dyn in
  function
  | Any -> variant "Any" []
  | Profile p -> variant "Profile" [ Profile.to_dyn p ]
;;

let equal_pattern x y =
  match x, y with
  | Profile x, Profile y -> Profile.equal x y
  | Any, Any -> true
  | _, _ -> false
;;

let hash_pattern = Poly.hash

type t =
  { loc : Loc.t
  ; rules : (pattern * config) list
  }

let hash { loc = _; rules } = List.hash (Tuple.T2.hash hash_pattern hash_config) rules

let to_dyn { rules; loc = _ } =
  let open Dyn in
  Dyn.list (pair dyn_of_pattern dyn_of_config) rules
;;

let equal { loc = _; rules } t =
  List.equal (Tuple.T2.equal equal_pattern equal_config) rules t.rules
;;

let inline_tests_field =
  field_o "inline_tests" (Syntax.since Stanza.syntax (1, 11) >>> Inline_tests.decode)
;;

let env_vars_field =
  field
    "env-vars"
    ~default:Env.empty
    (Syntax.since Stanza.syntax (1, 5)
     >>> located (repeat (pair string string))
     >>| fun (loc, pairs) ->
     match Env.Map.of_list pairs with
     | Ok vars -> Env.extend Env.empty ~vars
     | Error (k, _, _) ->
       User_error.raise ~loc [ Pp.textf "Variable %s is specified several times" k ])
;;

let odoc_field =
  field "odoc" ~default:Odoc.empty (Syntax.since Stanza.syntax (2, 4) >>> Odoc.decode)
;;

let menhir_field ~since =
  field_o "menhir" (Syntax.since Menhir.syntax since >>> Menhir_env.decode)
;;

let js_of_ocaml_field =
  field
    "js_of_ocaml"
    ~default:Js_of_ocaml.Env.empty
    (Syntax.since Stanza.syntax (3, 0) >>> Js_of_ocaml.Env.decode ~mode:JS)
;;

let wasm_of_ocaml_field =
  field
    "wasm_of_ocaml"
    ~default:Js_of_ocaml.Env.empty
    (Syntax.since Stanza.syntax (3, 17) >>> Js_of_ocaml.Env.decode ~mode:Wasm)
;;

let bin_annot = field_o "bin_annot" (Syntax.since Stanza.syntax (3, 8) >>> bool)

let config =
  let+ flags = Ocaml_flags.Spec.decode
  and+ foreign_flags = foreign_flags ~since:(Some (1, 7))
  and+ link_flags =
    Link_flags.Spec.decode ~check:(Some (Syntax.since Stanza.syntax (3, 0)))
  and+ env_vars = env_vars_field
  and+ binaries =
    field_o
      "binaries"
      (Syntax.since Stanza.syntax (1, 6) >>> File_binding.Unexpanded.L.decode)
  and+ inline_tests = inline_tests_field
  and+ menhir = menhir_field ~since:Menhir.explain_since
  and+ menhir_flags = menhir_flags ~since:(2, 1) ~deleted_in:Menhir.explain_since
  and+ odoc = odoc_field
  and+ js_of_ocaml = js_of_ocaml_field
  and+ wasm_of_ocaml = wasm_of_ocaml_field
  and+ coq = Coq_env.decode
  and+ rocq = Rocq_env.decode
  and+ format_config = Format_config.field ~since:(2, 8)
  and+ bin_annot = bin_annot in
  let menhir =
    match menhir_flags, menhir with
    | Some flags, None -> { Menhir_env.empty with flags }
    | None, Some env -> env
    | None, None -> Menhir_env.empty
    | Some _, Some _ ->
      Code_error.raise "(menhir_flags) and (menhir) cannot both be present" []
  in
  { flags
  ; foreign_flags
  ; link_flags
  ; env_vars
  ; binaries
  ; inline_tests
  ; menhir
  ; odoc
  ; js_of_ocaml
  ; wasm_of_ocaml
  ; coq
  ; rocq
  ; format_config
  ; error_on_use = None
  ; warn_on_load = None
  ; bin_annot
  }
;;

let rule =
  enter
    (let+ pat =
       keyword "_"
       >>> return Any
       <|> let+ p = Profile.decode in
           Profile p
     and+ configs = fields config in
     pat, configs)
;;

let check_rules ~version ~loc rules =
  let rec has_after_any = function
    | [ (Any, _) ] | [] -> false
    | (Any, _) :: _ :: _ -> true
    | (Profile _, _) :: rules -> has_after_any rules
  in
  if has_after_any rules
  then (
    let is_error = version >= (3, 4) in
    User_warning.emit
      ~loc
      ~is_error
      [ Pp.text
          "This env stanza contains rules after a wildcard rule. These are going to be \
           ignored."
      ])
;;

let decode =
  let+ () = Syntax.since Stanza.syntax (1, 0)
  and+ loc = loc
  and+ rules = repeat rule
  and+ version = Syntax.get_exn Stanza.syntax in
  check_rules ~version ~loc rules;
  { loc; rules }
;;

let empty = { loc = Loc.none; rules = [] }

let find_opt t ~profile =
  List.find_map t.rules ~f:(fun (pat, cfg) ->
    match pat with
    | Any -> Some cfg
    | Profile a -> Option.some_if (a = profile) cfg)
;;

let find t ~profile = Option.value ~default:empty_config (find_opt t ~profile)
let map_configs t ~f = { t with rules = List.map t.rules ~f:(fun (p, c) -> p, f c) }

let add_error t ~message =
  map_configs t ~f:(fun c -> { c with error_on_use = Some message })
;;

let add_warning t ~message =
  map_configs t ~f:(fun c -> { c with warn_on_load = Some message })
;;

include Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)
