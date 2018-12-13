open! Stdune

module type Keys = sig
  type t
  type elt
  val empty : t
  val add : t -> elt -> t
  val mem : t -> elt -> bool
end

module type S = sig
  type key
  type 'a monad
  val top_closure
    :  key:('a -> key)
    -> deps:('a -> 'a list monad)
    -> 'a list
    -> ('a list, 'a list) result monad
end

module Make(Keys : Keys)(Monad : Monad.S) = struct
  open Monad

  let top_closure ~key ~deps elements =
    let visited = ref Keys.empty in
    let res = ref [] in
    let rec loop elt ~temporarily_marked =
      let key = key elt in
      if Keys.mem temporarily_marked key then
        return (Error [elt])
      else if not (Keys.mem !visited key) then begin
        visited := Keys.add !visited key;
        let temporarily_marked = Keys.add temporarily_marked key in
        deps elt
        >>= iter_elts ~temporarily_marked
        >>= function
        | Ok () -> res := elt :: !res; return (Ok ())
        | Error l -> return (Error (elt :: l))
      end else
        return (Ok ())
    and iter_elts elts ~temporarily_marked =
      return elts
      >>= function
      | [] -> return (Ok ())
      | elt :: elts ->
        loop elt ~temporarily_marked
        >>= function
        | Error _ as result -> return result
        | Ok () -> iter_elts elts ~temporarily_marked
    in
    iter_elts elements ~temporarily_marked:Keys.empty
    >>= function
    | Ok () -> return (Ok (List.rev !res))
    | Error elts -> return (Error elts)
end [@@inlined always]

module Int    = Make(Int.Set)(Monad.Id)
module String = Make(String.Set)(Monad.Id)
