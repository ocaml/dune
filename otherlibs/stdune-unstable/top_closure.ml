module Make (Keys : Top_closure_intf.Keys) (Monad : Monad_intf.S) = struct
  open Monad.O

  let top_closure ~key ~deps elements =
    let visited = ref Keys.empty in
    let res = ref [] in
    let rec loop elt ~temporarily_marked =
      let key = key elt in
      if Keys.mem temporarily_marked key then
        Monad.return (Error [ elt ])
      else if not (Keys.mem !visited key) then (
        visited := Keys.add !visited key;
        let temporarily_marked = Keys.add temporarily_marked key in
        deps elt >>= iter_elts ~temporarily_marked >>= function
        | Ok () ->
          res := elt :: !res;
          Monad.return (Ok ())
        | Error l -> Monad.return (Error (elt :: l))
      ) else
        Monad.return (Ok ())
    and iter_elts elts ~temporarily_marked =
      Monad.return elts >>= function
      | [] -> Monad.return (Ok ())
      | elt :: elts -> (
        loop elt ~temporarily_marked >>= function
        | Error _ as result -> Monad.return result
        | Ok () -> iter_elts elts ~temporarily_marked)
    in
    iter_elts elements ~temporarily_marked:Keys.empty >>= function
    | Ok () -> Monad.return (Ok (List.rev !res))
    | Error elts -> Monad.return (Error elts)
end
[@@inlined always]

module Int = Make (Int.Set) (Monad.Id)
module String = Make (String.Set) (Monad.Id)
