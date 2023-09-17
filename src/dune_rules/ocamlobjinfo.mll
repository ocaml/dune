{
open Import

type t = Module_name.Unique.Set.t Ml_kind.Dict.t

let to_dyn = Ml_kind.Dict.to_dyn Module_name.Unique.Set.to_dyn

let empty =
  { Ml_kind.Dict.
    intf = Module_name.Unique.Set.empty
  ; impl = Module_name.Unique.Set.empty
  }

let add_intf (t : t) i =
  { t with intf = Module_name.Unique.Set.add t.intf (Module_name.Unique.of_string i) }
let add_impl (t : t) i =
  { t with impl = Module_name.Unique.Set.add t.impl (Module_name.Unique.of_string i) }
}

let newline = '\r'? '\n'
let ws = [' ' '\t']+
let hash = ['0'-'9' 'a'-'z' '-']+
let name = ['A'-'Z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

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
let parse s = ocamlobjinfo empty (Lexing.from_string s)

let rules (ocaml : Ocaml_toolchain.t) ~dir ~sandbox ~unit =
  let output =
    Path.Build.relative dir (Path.basename unit)
    |> Path.Build.extend_basename ~suffix:".ooi-deps"
  in
  let no_approx =
    if Ocaml.Version.ooi_supports_no_approx ocaml.version then
      [Command.Args.A "-no-approx"]
    else
      []
  in
  let no_code =
    if Ocaml.Version.ooi_supports_no_code ocaml.version then
      [Command.Args.A "-no-code"]
    else
      []
  in
  ( Command.run ?sandbox
      ~dir:(Path.build dir) ocaml.ocamlobjinfo
      (List.concat
         [ no_approx
         ; no_code
         ; [ Dep unit ]
         ])
      ~stdout_to:output
  , Action_builder.map ~f:parse (Action_builder.contents (Path.build output))
  )
}
