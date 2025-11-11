open Import

let toolchains = Config.make_toggle ~name:"toolchains" ~default:Setup.toolchains
let lock_dev_tools = Config.make_toggle ~name:"lock_dev_tool" ~default:Setup.lock_dev_tool
let bin_dev_tools = Config.make_toggle ~name:"bin_dev_tools" ~default:Setup.bin_dev_tools

let portable_lock_dir =
  Config.make_toggle ~name:"portable_lock_dir" ~default:Setup.portable_lock_dir
;;
