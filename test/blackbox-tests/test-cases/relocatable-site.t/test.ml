Format.printf "%a@."
  (Format.pp_print_list ~pp_sep:Format.pp_print_space Format.pp_print_string)
  Site.Sites.contents;;
