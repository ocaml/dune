open! Import

let all =
  let open Dune_lang.Decoder in
  [ ( "generate_sites_module"
    , let+ t = Generate_sites_module_stanza.decode in
      [ Generate_sites_module_stanza.make_stanza t ] )
  ; ( "plugin"
    , let+ t = Plugin.decode in
      [ Plugin.make_stanza t ] )
  ]
;;
