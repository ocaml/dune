{
open Stdune

type t = Module.Name.Set.t Ml_kind.Dict.t

let empty =
  { Ml_kind.Dict.
    intf = Module.Name.Set.empty
  ; impl = Module.Name.Set.empty
  }

let add_intf (t : t) i =
  { t with intf = Module.Name.Set.add t.intf (Module.Name.of_string i) }
let add_impl (t : t) i =
  { t with impl = Module.Name.Set.add t.impl (Module.Name.of_string i) }
}

let newline = '\r'? '\n'
let ws = [' ' '\t']+
let hash = ['0'-'9' 'a'-'z']+
let name = ['A'-'Z'] ['a'-'z' '0'-'9' '_']*

rule ocamlobjinfo acc = parse
  | "Interfaces imported:" newline { intfs acc lexbuf }
  | "Implementations imported:" newline { impls acc lexbuf }
  | _ { ocamlobjinfo acc lexbuf }
  | eof { acc }
and intfs acc = parse
  | ws hash ws (name as name) newline { intfs (add_intf acc name) lexbuf }
  | "Implementations imported:" newline { impls acc lexbuf }
  | _ | eof { acc }
and impls acc = parse
  | ws hash ws (name as name) newline { impls (add_impl acc name) lexbuf }
  | _ | eof { acc }

{
let load ~ocamlobjinfo:bin ~unit =
  ["-no-approx"; "-no-code"; Path.to_absolute_filename unit]
  |> Process.run_capture ~env:Env.empty Process.Strict bin
  |> Fiber.map ~f:(fun s -> ocamlobjinfo empty (Lexing.from_string s))
}
