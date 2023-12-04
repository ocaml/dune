module Make (Keys : Top_closure_intf.Keys) (Monad : Monad_intf.S) = struct
  open Monad.O

  let top_closure ~key ~deps elements =
    let rec loop res visited elt ~temporarily_marked =
      let key = key elt in
      if Keys.mem temporarily_marked key
      then Monad.return (Error [ elt ])
      else if not (Keys.mem visited key)
      then (
        let visited = Keys.add visited key in
        let temporarily_marked = Keys.add temporarily_marked key in
        deps elt
        >>= iter_elts res visited ~temporarily_marked
        >>| function
        | Error l -> Error (elt :: l)
        | Ok (res, visited) ->
          let res = elt :: res in
          Ok (res, visited))
      else Monad.return (Ok (res, visited))
    and iter_elts res visited elts ~temporarily_marked =
      match elts with
      | [] -> Monad.return (Ok (res, visited))
      | elt :: elts ->
        loop res visited elt ~temporarily_marked
        >>= (function
         | Error _ as result -> Monad.return result
         | Ok (res, visited) -> iter_elts res visited elts ~temporarily_marked)
    in
    iter_elts [] Keys.empty elements ~temporarily_marked:Keys.empty
    >>| function
    | Ok (res, _visited) -> Ok (List.rev res)
    | Error elts -> Error elts
  ;;
end
[@@inline always]

module Int = Make (Int.Set) (Monad.Id)
module String = Make (String.Set) (Monad.Id)
