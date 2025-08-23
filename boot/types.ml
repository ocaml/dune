type root_module =
  { name : string
  ; entries : string list
  }

type library =
  { path : string
  ; main_module_name : string option
  ; include_subdirs_unqualified : bool
  ; special_builtin_support : string option
  ; root_module : root_module option
  }
