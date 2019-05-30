{
open Stdune

type t = Module.Name.Set.t Ml_kind.Dict.t

let pp =
  Ml_kind.Dict.pp Module.Name.Set.pp

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

let rules ~dir ~(ctx : Context.t) ~unit =
  let open Build.O in
  let output =
    Path.Build.relative dir (Path.basename unit)
    |> Path.Build.extend_basename ~suffix:".ooi-deps"
  in
  let bin =
    match ctx.ocamlobjinfo with
    | None ->
      Error (
        let context = Context.name ctx in
        let program = "ocamlobjinfo" in
        Action.Prog.Not_found.create ~context ~program ~loc:None ())
    | Some bin -> Ok bin
  in
  let no_approx =
    if Ocaml_version.ooi_supports_no_approx ctx.version then
      [Command.Args.A "-no-approx"]
    else
      []
  in
  let no_code =
    if Ocaml_version.ooi_supports_no_code ctx.version then
      [Command.Args.A "-no-code"]
    else
      []
  in
  ( Command.run ~dir:(Path.build dir) bin
      (List.concat
         [ no_approx
         ; no_code
         ; [ Dep unit ]
         ])
      ~stdout_to:output
  , Build.contents (Path.build output) >>^ parse
  )
}
