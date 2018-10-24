open Stdune

type t =
  { dir : Path.t
  ; env : Env.t
  ; ocaml_config : Value.t list String.Map.t
  ; bindings : Pform.Map.t
  ; scope : Scope.t
  }

let ectx t = { Action.Unexpanded. dir = t.dir; env = t.env }
let scope t = t.scope
let dir t = t.dir
let bindings t = t.bindings

let make_ocaml_config ocaml_config =
  let string s = [Value.String s] in
  Ocaml_config.to_list ocaml_config
  |> List.map  ~f:(fun (k, v) ->
    ( k
    , match (v : Ocaml_config.Value.t) with
    | Bool          x -> string (string_of_bool x)
    | Int           x -> string (string_of_int x)
    | String        x -> string x
    | Words         x -> Value.L.strings x
    | Prog_and_args x -> Value.L.strings (x.prog :: x.args)))
  |> String.Map.of_list_exn

let make ~scope ~(context : Context.t) ~cxx_flags =
  let ocaml_config = make_ocaml_config context.ocaml_config in
  let dir = context.build_dir in
  let bindings = Pform.Map.create ~context ~cxx_flags in
  let env = context.env in
  { dir
  ; env
  ; ocaml_config
  ; bindings
  ; scope
  }

let set_env t ~var ~value =
  { t with env = Env.add t.env ~var ~value }

let set_dir t ~dir =
  { t with dir }

let update t ~dir ~scope ~env ~add_bindings =
  { t with
    dir
  ; env
  ; scope
  ; bindings = Pform.Map.superpose t.bindings add_bindings
  }

let expand_ocaml_config ocaml_config pform name =
  match String.Map.find ocaml_config name with
  | Some x -> x
  | None ->
    Errors.fail (String_with_vars.Var.loc pform)
      "Unknown ocaml configuration variable %S"
      name

let expand_env ~env pform s : Value.t list =
  match String.rsplit2 s ~on:'=' with
  | None ->
    Errors.fail (String_with_vars.Var.loc pform)
      "%s must always come with a default value\n\
       Hint: the syntax is %%{env:VAR=DEFAULT-VALUE}"
      (String_with_vars.Var.describe pform)
  | Some (var, default) ->
    [String (Option.value ~default (Env.get env var))]

let expand_var { bindings; ocaml_config; env; scope; dir = _ }
      var syntax_version =
  Pform.Map.expand bindings var syntax_version
  |> Option.map ~f:(function
    | Pform.Expansion.Var (Values l) -> Ok l
    | Macro (Ocaml_config, s) ->
      Ok (expand_ocaml_config ocaml_config var s)
    | Macro (Env, s) -> Ok (expand_env ~env var s)
    | Var Project_root -> Ok [Value.Dir (Scope.root scope)]
    | expansion -> Error expansion)

let expand_var_exn t var syn =
  expand_var t var syn
  |> Option.map ~f:(function
    | Ok s -> s
    | Error _ ->
      Errors.fail (String_with_vars.Var.loc var)
        "%s isn't allowed in this position"
        (String_with_vars.Var.describe var))

let expand_static t ~mode ~template =
  String_with_vars.expand ~dir:t.dir ~mode template ~f:(fun var syn ->
    expand_var_exn t var syn)
