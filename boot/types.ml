type 'name root_module =
  { name : 'name
  ; entries : 'name list
  }

type include_subdirs =
  | Unqualified
  | Qualified of (string list * string list) list
  | No

type 'name library =
  { path : string
  ; main_module_name : 'name option
  ; include_subdirs : include_subdirs
  ; special_builtin_support : 'name option
  ; root_module : 'name root_module option
  }
