type root_module =
  { name : string
  ; entries : string list
  }

type include_subdirs =
  | Unqualified
  | Qualified
  | No

type library =
  { path : string
  ; main_module_name : string option
  ; include_subdirs : include_subdirs
  ; special_builtin_support : string option
  ; root_module : root_module option
  }
