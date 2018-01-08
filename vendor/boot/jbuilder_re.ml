module Re = struct
  type t = unit
  type re = unit
  let compile () = ()
  let execp _ _ = false
  let group _ = ()
  let any _ = ()
  let rep1 _ = ()
  let char _ = ()
  let compl _ = ()
  let str _ = ()
  let seq _ = ()
  let exec_opt _ _ = None

  module Group = struct
    let get _ _ = ""
  end
end
