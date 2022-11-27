open Import

type stanza = Stanza.t = ..

module Stanza = struct
  open Dune_lang.Decoder

  let foreign_flags ~since =
    let check =
      Option.map since ~f:(fun since ->
          Dune_lang.Syntax.since Stanza.syntax since)
    in
    let+ c = Ordered_set_lang.Unexpanded.field "c_flags" ?check
    and+ cxx = Ordered_set_lang.Unexpanded.field "cxx_flags" ?check in
    Foreign_language.Dict.make ~c ~cxx

  let menhir_flags ~since =
    let check =
      Option.map since ~f:(fun since ->
          Dune_lang.Syntax.since Menhir_stanza.syntax since)
    in
    Ordered_set_lang.Unexpanded.field "menhir_flags" ?check

  module Inline_tests = struct
    type t =
      | Enabled
      | Disabled
      | Ignored

    let equal x y =
      match (x, y) with
      | Enabled, Enabled -> true
      | Disabled, Disabled -> true
      | Ignored, Ignored -> true
      | _, _ -> false

    let decode =
      enum
        [ ("enabled", Enabled); ("disabled", Disabled); ("ignored", Ignored) ]

    let to_string = function
      | Enabled -> "enabled"
      | Disabled -> "disabled"
      | Ignored -> "ignored"
  end

  module Odoc = struct
    type warnings =
      | Fatal
      | Nonfatal

    type t = { warnings : warnings option }

    let empty = { warnings = None }

    let warnings_equal x y =
      match (x, y) with
      | Fatal, Fatal | Nonfatal, Nonfatal -> true
      | (Fatal | Nonfatal), _ -> false

    let equal x y = Option.equal warnings_equal x.warnings y.warnings

    let warnings_decode = enum [ ("fatal", Fatal); ("nonfatal", Nonfatal) ]

    let decode =
      fields
      @@ let+ warnings = field_o "warnings" warnings_decode in
         { warnings }
  end

  type config =
    { flags : Ocaml_flags.Spec.t
    ; foreign_flags : Ordered_set_lang.Unexpanded.t Foreign_language.Dict.t
    ; link_flags : Link_flags.Spec.t
    ; env_vars : Env.t
    ; binaries : File_binding.Unexpanded.t list
    ; inline_tests : Inline_tests.t option
    ; menhir_flags : Ordered_set_lang.Unexpanded.t
    ; odoc : Odoc.t
    ; js_of_ocaml : Ordered_set_lang.Unexpanded.t Js_of_ocaml.Env.t
    ; coq : Ordered_set_lang.Unexpanded.t
    ; format_config : Format_config.t option
    ; error_on_use : User_message.t option
    ; warn_on_load : User_message.t option
    }

  let equal_config
      { flags
      ; foreign_flags
      ; link_flags
      ; env_vars
      ; binaries
      ; inline_tests
      ; menhir_flags
      ; odoc
      ; js_of_ocaml
      ; coq
      ; format_config
      ; error_on_use
      ; warn_on_load
      } t =
    Ocaml_flags.Spec.equal flags t.flags
    && Foreign_language.Dict.equal Ordered_set_lang.Unexpanded.equal
         foreign_flags t.foreign_flags
    && Link_flags.Spec.equal link_flags t.link_flags
    && Env.equal env_vars t.env_vars
    && List.equal File_binding.Unexpanded.equal binaries t.binaries
    && Option.equal Inline_tests.equal inline_tests t.inline_tests
    && Ordered_set_lang.Unexpanded.equal menhir_flags t.menhir_flags
    && Odoc.equal odoc t.odoc
    && Ordered_set_lang.Unexpanded.equal coq t.coq
    && Option.equal Format_config.equal format_config t.format_config
    && Js_of_ocaml.Env.equal js_of_ocaml t.js_of_ocaml
    && Option.equal User_message.equal error_on_use t.error_on_use
    && Option.equal User_message.equal warn_on_load t.warn_on_load

  let hash_config = Poly.hash

  let empty_config =
    { flags = Ocaml_flags.Spec.standard
    ; foreign_flags =
        Foreign_language.Dict.make_both Ordered_set_lang.Unexpanded.standard
    ; link_flags = Link_flags.Spec.standard
    ; env_vars = Env.empty
    ; binaries = []
    ; inline_tests = None
    ; menhir_flags = Ordered_set_lang.Unexpanded.standard
    ; odoc = Odoc.empty
    ; js_of_ocaml = Js_of_ocaml.Env.empty
    ; coq = Ordered_set_lang.Unexpanded.standard
    ; format_config = None
    ; error_on_use = None
    ; warn_on_load = None
    }

  type pattern =
    | Profile of Profile.t
    | Any

  let equal_pattern x y =
    match (x, y) with
    | Profile x, Profile y -> Profile.equal x y
    | Any, Any -> true
    | _, _ -> false

  let hash_pattern = Poly.hash

  type t =
    { loc : Loc.t
    ; rules : (pattern * config) list
    }

  let hash { loc = _; rules } =
    List.hash (Tuple.T2.hash hash_pattern hash_config) rules

  let to_dyn = Dyn.opaque

  let equal { loc = _; rules } t =
    List.equal (Tuple.T2.equal equal_pattern equal_config) rules t.rules

  let inline_tests_field =
    field_o "inline_tests"
      (Dune_lang.Syntax.since Stanza.syntax (1, 11) >>> Inline_tests.decode)

  let env_vars_field =
    field "env-vars" ~default:Env.empty
      ( Dune_lang.Syntax.since Stanza.syntax (1, 5)
      >>> located (repeat (pair string string))
      >>| fun (loc, pairs) ->
        match Env.Map.of_list pairs with
        | Ok vars -> Env.extend Env.empty ~vars
        | Error (k, _, _) ->
          User_error.raise ~loc
            [ Pp.textf "Variable %s is specified several times" k ] )

  let odoc_field =
    field "odoc" ~default:Odoc.empty
      (Dune_lang.Syntax.since Stanza.syntax (2, 4) >>> Odoc.decode)

  let js_of_ocaml_field =
    field "js_of_ocaml" ~default:Js_of_ocaml.Env.empty
      (Dune_lang.Syntax.since Stanza.syntax (3, 0) >>> Js_of_ocaml.Env.decode)

  let coq_flags = Ordered_set_lang.Unexpanded.field "flags"

  let coq_field =
    field "coq" ~default:Ordered_set_lang.Unexpanded.standard
      (Dune_lang.Syntax.since Stanza.syntax (2, 7) >>> fields coq_flags)

  let config =
    let+ flags = Ocaml_flags.Spec.decode
    and+ foreign_flags = foreign_flags ~since:(Some (1, 7))
    and+ link_flags = Link_flags.Spec.decode ~since:(Some (3, 0))
    and+ env_vars = env_vars_field
    and+ binaries =
      field ~default:[] "binaries"
        (Dune_lang.Syntax.since Stanza.syntax (1, 6)
        >>> File_binding.Unexpanded.L.decode)
    and+ inline_tests = inline_tests_field
    and+ menhir_flags = menhir_flags ~since:(Some (2, 1))
    and+ odoc = odoc_field
    and+ js_of_ocaml = js_of_ocaml_field
    and+ coq = coq_field
    and+ format_config = Format_config.field ~since:(2, 8) in
    { flags
    ; foreign_flags
    ; link_flags
    ; env_vars
    ; binaries
    ; inline_tests
    ; menhir_flags
    ; odoc
    ; js_of_ocaml
    ; coq
    ; format_config
    ; error_on_use = None
    ; warn_on_load = None
    }

  let rule =
    enter
      (let+ pat =
         keyword "_" >>> return Any
         <|> let+ p = Profile.decode in
             Profile p
       and+ configs = fields config in
       (pat, configs))

  let check_rules ~version ~loc rules =
    let rec has_after_any = function
      | [ (Any, _) ] | [] -> false
      | (Any, _) :: _ :: _ -> true
      | (Profile _, _) :: rules -> has_after_any rules
    in
    if has_after_any rules then
      let is_error = version >= (3, 4) in
      User_warning.emit ~loc ~is_error
        [ Pp.text
            "This env stanza contains rules after a wildcard rule. These are \
             going to be ignored."
        ]

  let decode =
    let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 0)
    and+ loc = loc
    and+ rules = repeat rule
    and+ version = Dune_lang.Syntax.get_exn Stanza.syntax in
    check_rules ~version ~loc rules;
    { loc; rules }

  let empty = { loc = Loc.none; rules = [] }

  let find t ~profile =
    Option.value ~default:empty_config
    @@ List.find_map t.rules ~f:(fun (pat, cfg) ->
           match pat with
           | Any -> Some cfg
           | Profile a -> Option.some_if (a = profile) cfg)

  let map_configs t ~f =
    { t with rules = List.map t.rules ~f:(fun (p, c) -> (p, f c)) }

  let add_error t ~message =
    map_configs t ~f:(fun c -> { c with error_on_use = Some message })

  let add_warning t ~message =
    map_configs t ~f:(fun c -> { c with warn_on_load = Some message })

  let fire_hooks t ~profile =
    let current_config = find t ~profile in
    let message_contents (msg : User_message.t) = (msg.loc, msg.paragraphs) in
    Option.iter current_config.error_on_use ~f:(fun msg ->
        let loc, paragraphs = message_contents msg in
        User_error.raise ?loc paragraphs);
    List.iter t.rules ~f:(fun (_, config) ->
        Option.iter config.warn_on_load ~f:(fun msg ->
            let loc, paragraphs = message_contents msg in
            User_warning.emit ?loc paragraphs))
end

type stanza += T of Stanza.t
