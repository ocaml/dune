{
open! Stdune
}

rule is_script = parse
  | "(* -*- tuareg -*- *)" { true }
  | ""                     { false }

and eof_reached = parse
  | eof { true  }
  | ""  { false }
