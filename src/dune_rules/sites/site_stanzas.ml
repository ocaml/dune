open! Import

let all =
  [ ("generate_sites_module", Generate_sites_module_stanza.(decode_stanza decode))
  ; ("plugin", Plugin.(decode_stanza Plugin.decode))
  ]
;;
