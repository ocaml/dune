(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016 MetaStack Solutions Ltd.                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

rule main unquoted quoted = parse
| [^ '"' '\n' ]+
               { unquoted @@ Lexing.lexeme lexbuf;
                 main unquoted quoted lexbuf }
| ("\"\"\"" | '"') as quote
               { unquoted quote;
                 let triple = String.length quote = 3 in
                 string triple unquoted quoted lexbuf }
| '\n'         { Lexing.new_line lexbuf;
                 unquoted "\n";
                 main unquoted quoted lexbuf }
| eof          { () }


and string triple unquoted quoted = parse
| ( [^ '"' '\\' '\n' ]+ | '\\' [^ '\n' ]? )+
               { quoted @@ Lexing.lexeme lexbuf;
                 string triple unquoted quoted lexbuf }
| "\"\"\""
               { unquoted "\"\"\"";
                 main unquoted quoted lexbuf }
| '"'          { if triple then begin
                   quoted "\"";
                   string triple unquoted quoted lexbuf
                 end else begin
                   unquoted "\"";
                   main unquoted quoted lexbuf
                 end }
| '\n'         { Lexing.new_line lexbuf;
                 unquoted "\n";
                 string triple unquoted quoted lexbuf}
| eof          { () }
