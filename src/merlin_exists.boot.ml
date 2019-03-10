open Stdune

let dep ~dir:_ = Build.arr (fun x -> x)
let create ~dir:_ = Build.arr (fun _ -> Action.Progn [])
