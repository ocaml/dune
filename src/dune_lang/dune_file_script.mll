{
}

rule is_script = parse
  | "(* -*- tuareg -*- *)" { true }
  | eof {
      (* Keep direct-code generation's entry points mutually recursive. *)
      not (eof_reached lexbuf)
    }
  | ""                     { false }

and eof_reached = parse
  | eof { true  }
  | ""  { false }
