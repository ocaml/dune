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

  type config =
    { flags          : Ocaml_flags.Spec.t
    ; c_flags        : Ordered_set_lang.Unexpanded.t C.Kind.Dict.t
    ; env_vars       : Env.t
    ; binaries       : File_binding.Unexpanded.t list
    }

  type pattern =
    | Profile of string
    | Any

  type t =
    { loc   : Loc.t
    ; rules : (pattern * config) list
    }

  let env_vars_field =
    field
      "env-vars"
      ~default:Env.empty
      (Syntax.since Stanza.syntax (1, 5) >>>
       located (list (pair string string)) >>| fun (loc, pairs) ->
       match Env.Map.of_list pairs with
       | Ok vars -> Env.extend Env.empty ~vars
       | Error (k, _, _) ->
         Errors.fail loc "Variable %s is specified several times" k)

  let config =
    let+ flags = Ocaml_flags.Spec.decode
    and+ c_flags = c_flags ~since:(Some (1, 7))
    and+ env_vars = env_vars_field
    and+ binaries = field ~default:[] "binaries"
                      (Syntax.since Stanza.syntax (1, 6)
                       >>> File_binding.Unexpanded.L.decode)
    in
    { flags
    ; c_flags
    ; env_vars
    ; binaries
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

  let find t ~profile =
    List.find_map t.rules ~f:(fun (pat, cfg) ->
      match pat with
      | Any -> Some cfg
      | Profile a -> Option.some_if (a = profile) cfg)

end

type stanza +=
  | T of Stanza.t
