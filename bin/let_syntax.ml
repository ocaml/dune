let ( let+ ) t f = Cmdliner.Term.(const f $ t)
let ( and+ ) a b = Cmdliner.Term.(const (fun x y -> x, y) $ a $ b)
