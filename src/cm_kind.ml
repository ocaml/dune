type t = Cmi | Cmo | Cmx

let all = [Cmi; Cmo; Cmx]

let choose cmi cmo cmx = function
  | Cmi -> cmi
  | Cmo -> cmo
  | Cmx -> cmx

let ext = choose ".cmi" ".cmo" ".cmx"

let compiler t (ctx : Context.t) =
  choose (Some ctx.ocamlc) (Some ctx.ocamlc) ctx.ocamlopt t

let source = choose Ml_kind.Intf Impl Impl
