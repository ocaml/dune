open Dune_config

let is_enabled =
  lazy
    (match Config.get Dune_rules.Compile_time.bin_dev_tools with
     | `Enabled -> true
     | `Disabled -> false)
;;
