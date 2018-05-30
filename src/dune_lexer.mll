rule is_script = parse
  | "(* -*- tuareg -*- *)" { true }
  | ""                     { false }
