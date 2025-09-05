open Import

val validate_for_install_stanza
  :  File_binding.Expanded.t
  -> relative_dst_path_starts_with_parent_error_when:
       [ `Deprecation_warning_from_3_11 | `Always_error ]
  -> unit Memo.t

val expand
  :  File_binding.Unexpanded.t
  -> dir:Path.Build.t
  -> f:(String_with_vars.t -> string Memo.t)
  -> File_binding.Expanded.t Memo.t

val expand_src
  :  File_binding.Unexpanded.t
  -> dir:Path.Build.t
  -> f:(String_with_vars.t -> string Memo.t)
  -> Path.Build.t Memo.t

val destination_relative_to_install_path
  :  File_binding.Unexpanded.t
  -> section:Section.t
  -> expand:(String_with_vars.t -> string Memo.t)
  -> expand_partial:(String_with_vars.t -> String_with_vars.t Memo.t)
  -> Install.Entry.Dst.t Memo.t
