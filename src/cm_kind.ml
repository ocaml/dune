type t = Cmi | Cmo | Cmx

let all = [Cmi; Cmo; Cmx]

let choose cmi cmo cmx = function
  | Cmi -> cmi
  | Cmo -> cmo
  | Cmx -> cmx

let ext = choose ".cmi" ".cmo" ".cmx"

let source = choose Ml_kind.Intf Impl Impl
