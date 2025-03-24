open Import

let toolchains = Config.make_toggle ~name:"toolchains" ~default:Setup.toolchains

let pkg_build_progress =
  Config.make_toggle ~name:"pkg_build_progress" ~default:Setup.pkg_build_progress
;;

let lock_dev_tools = Config.make_toggle ~name:"lock_dev_tool" ~default:Setup.lock_dev_tool
