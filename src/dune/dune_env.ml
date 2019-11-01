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
    Foreign.Language.Dict.make ~c ~cxx

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

  type config =
    { flags : Ocaml_flags.Spec.t
    ; foreign_flags : Ordered_set_lang.Unexpanded.t Foreign.Language.Dict.t
    ; env_vars : Env.t
    ; binaries : File_binding.Unexpanded.t list
    ; inline_tests : Inline_tests.t option
    }

  let equal_config { flags; foreign_flags; env_vars; binaries; inline_tests } t
      =
    Ocaml_flags.Spec.equal flags t.flags
    && Foreign.Language.Dict.equal Ordered_set_lang.Unexpanded.equal
         foreign_flags t.foreign_flags
    && Env.equal env_vars t.env_vars
    && List.equal File_binding.Unexpanded.equal binaries t.binaries
    && Option.equal Inline_tests.equal inline_tests t.inline_tests

  let empty_config =
    { flags = Ocaml_flags.Spec.standard
    ; foreign_flags =
        Foreign.Language.Dict.make_both Ordered_set_lang.Unexpanded.standard
    ; env_vars = Env.empty
    ; binaries = []
    ; inline_tests = None
    }

  type pattern =
    | Profile of Profile.t
    | Any

  let equal_pattern x y =
    match (x, y) with
    | Profile x, Profile y -> Profile.equal x y
    | Any, Any -> true
    | _, _ -> false

  type t =
    { loc : Loc.t
    ; rules : (pattern * config) list
    }

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

  let config =
    let+ flags = Ocaml_flags.Spec.decode
    and+ foreign_flags = foreign_flags ~since:(Some (1, 7))
    and+ env_vars = env_vars_field
    and+ binaries =
      field ~default:[] "binaries"
        ( Dune_lang.Syntax.since Stanza.syntax (1, 6)
        >>> File_binding.Unexpanded.L.decode )
    and+ inline_tests = inline_tests_field in
    { flags; foreign_flags; env_vars; binaries; inline_tests }

  let rule =
    enter
      (let+ pat =
         match_keyword
           [ ("_", return Any) ]
           ~fallback:
             (let+ p = Profile.decode in
              Profile p)
       and+ configs = fields config in
       (pat, configs))

  let decode =
    let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 0)
    and+ loc = loc
    and+ rules = repeat rule in
    { loc; rules }

  let empty = { loc = Loc.none; rules = [] }

  let find t ~profile =
    Option.value ~default:empty_config
    @@ List.find_map t.rules ~f:(fun (pat, cfg) ->
           match pat with
           | Any -> Some cfg
           | Profile a -> Option.some_if (a = profile) cfg)
end

type stanza += T of Stanza.t
