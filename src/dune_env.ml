open Import

type stanza = Stanza.t = ..

module Stanza = struct
  open Stanza.Decoder
  let c_flags ~since =
    let check =
      Option.map since ~f:(fun since ->
        Syntax.since Stanza.syntax since)
    in
    let+ c = Ordered_set_lang.Unexpanded.field "c_flags" ?check
    and+ cxx = Ordered_set_lang.Unexpanded.field "cxx_flags" ?check
    in
    C.Kind.Dict.make ~c ~cxx

  module Inline_tests = struct
    type t =
      | Enabled
      | Disabled
      | Ignored

    let decode =
      enum
        [ "enabled", Enabled
        ; "disabled", Disabled
        ; "ignored", Ignored ]

    let to_string = function
      | Enabled -> "enabled"
      | Disabled -> "disabled"
      | Ignored -> "ignored"

  end

  type config =
    { flags          : Ocaml_flags.Spec.t
    ; c_flags        : Ordered_set_lang.Unexpanded.t C.Kind.Dict.t
    ; env_vars       : Env.t
    ; paths          : (string * Ordered_set_lang.t) list
    ; binaries       : File_binding.Unexpanded.t list
    ; inline_tests   : Inline_tests.t option
    }

  type pattern =
    | Profile of string
    | Any

  type t =
    { loc   : Loc.t
    ; rules : (pattern * config) list
    }

  let inline_tests_field =
    field_o
    "inline_tests"
      (Syntax.since Stanza.syntax (1, 11) >>>
      Inline_tests.decode)

  let env_vars_field =
    field
    "env-vars"
      ~default:Env.empty
      (Syntax.since Stanza.syntax (1, 5) >>>
       located (list (pair string string)) >>| fun (loc, pairs) ->
       match Env.Map.of_list pairs with
       | Ok vars -> Env.extend Env.empty ~vars
       | Error (k, _, _) ->
         User_error.raise ~loc
           [ Pp.textf "Variable %s is specified several times" k ])

  let paths_field =
    let f l =
      match Env.Map.of_list (List.map ~f:(fun ((loc, s), _) -> s, loc) l) with
      | Ok _ ->
        List.map ~f:(fun ((_, s), x) -> s, x) l
      | Error (var, _, loc) ->
        User_error.raise ~loc
          [ Pp.textf "the variable %S can appear at most once \
                      in this stanza." var
          ]
    in
    field "paths" ~default:[]
      (Syntax.since Stanza.syntax (1, 12) >>>
       map ~f (list (pair (located string) Ordered_set_lang.decode)))

  let config =
    let+ flags = Ocaml_flags.Spec.decode
    and+ c_flags = c_flags ~since:(Some (1, 7))
    and+ env_vars = env_vars_field
    and+ paths = paths_field
    and+ binaries = field ~default:[] "binaries"
                      (Syntax.since Stanza.syntax (1, 6)
                       >>> File_binding.Unexpanded.L.decode)
    and+ inline_tests = inline_tests_field
    in
    { flags
    ; c_flags
    ; env_vars
    ; paths
    ; binaries
    ; inline_tests
    }

  let rule =
    enter
      (let+ pat =
         match_keyword [("_", return Any)]
           ~fallback:(string >>| fun s -> Profile s)
       and+ configs = fields config
       in
       (pat, configs))

  let decode =
    let+ () = Syntax.since Stanza.syntax (1, 0)
    and+ loc = loc
    and+ rules = repeat rule
    in
    { loc; rules }

  let empty = { loc = Loc.none; rules = [] }

  let find t ~profile =
    List.find_map t.rules ~f:(fun (pat, cfg) ->
      match pat with
      | Any -> Some cfg
      | Profile a -> Option.some_if (a = profile) cfg)

  let paths t ~dir ~profile ~default_env =
    let t = match find t ~profile with | None -> [] | Some c -> c.paths in
    let module Eval =
      Ordered_set_lang.Make(String)
        (struct
          type t = string
          type key = string
          let key x = x
        end)
    in
    let t =
      let f (var, t) =
        let parse ~loc:_ s = s in
        let standard = Env.path default_env |> List.map ~f:Path.to_string in
        var, Eval.eval t ~parse ~standard
      in
      List.map ~f t
    in
    let vars =
      let to_absolute_filename s =
        if Filename.is_relative s then
          Path.Source.relative dir s |> Path.source |> Path.to_absolute_filename
        else
          s
      in
      let env = Env.Map.of_list_exn t in
      let f l = String.concat ~sep:Bin.path_sep_s
                  (List.map ~f:to_absolute_filename l)
      in
      Env.Map.map ~f env
    in
    vars

  let env_vars t ~profile =
    match find t ~profile with | None ->Env.empty | Some c -> c.env_vars


end

type stanza +=
  | T of Stanza.t
