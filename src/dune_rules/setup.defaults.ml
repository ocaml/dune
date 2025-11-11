let library_path = []

let roots : string option Install.Roots.t =
  { lib_root = None
  ; man = None
  ; doc_root = None
  ; etc_root = None
  ; share_root = None
  ; bin = None
  ; sbin = None
  ; libexec_root = None
  }

let prefix : string option = None
let toolchains = `Enabled
let lock_dev_tool = `Disabled
let bin_dev_tools = `Disabled
let portable_lock_dir = `Disabled
