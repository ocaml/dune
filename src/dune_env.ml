open Import

type stanza = Stanza.t = ..

module Stanza = struct
  open Stanza.Decoder

  let field_oslu name = Ordered_set_lang.Unexpanded.field name

  type config =
    { flags          : Ordered_set_lang.Unexpanded.t
    ; ocamlc_flags   : Ordered_set_lang.Unexpanded.t
    ; ocamlopt_flags : Ordered_set_lang.Unexpanded.t
    ; env_vars       : Env.t
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
    let%map flags = field_oslu "flags"
    and ocamlc_flags = field_oslu "ocamlc_flags"
    and ocamlopt_flags = field_oslu "ocamlopt_flags"
    and env_vars = env_vars_field
    in
    { flags; ocamlc_flags; ocamlopt_flags; env_vars }

  let rule =
    enter
      (let%map pat =
         match_keyword [("_", return Any)]
           ~fallback:(string >>| fun s -> Profile s)
       and configs = fields config
       in
       (pat, configs))

  let decode =
    let%map () = Syntax.since Stanza.syntax (1, 0)
    and loc = loc
    and rules = repeat rule
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
