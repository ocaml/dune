module Plugin = Dune_rules_plugin_lwt
module Sexp = Plugin.Sexp

let shellcheck_fields =
  Sexp.Parser.parse_string
    ~fname:"shellcheck_rules.ml"
    ~mode:Many
    {|
(alias runtest)
(enabled_if %{bin-available:shellcheck})
(action (run %{bin:shellcheck} -s bash %{deps}))
|}
  |> List.map Sexp.Ast.remove_locs
;;

let rules = function
  | [] -> invalid_arg "Expected (plugin.shellcheck <script> ...)"
  | scripts ->
    [ Sexp.List
        (Sexp.atom "rule" :: List (Sexp.atom "deps" :: scripts) :: shellcheck_fields)
    ]
;;

let () = Lwt_main.run (Plugin.run (Lwt.wrap1 rules))
