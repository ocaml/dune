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
  val top_closure
    :  key:('a -> key)
    -> deps:('a -> 'a list)
    -> 'a list
    -> ('a list, 'a list) result
  val top_closure_f
    :  key:('a -> key Fiber.t)
    -> deps:('a -> 'a list Fiber.t)
    -> 'a list Fiber.t
    -> ('a list, 'a list) result Fiber.t
end

module type Monad = sig
  type 'a t
  val (>>=)  : 'a t -> ('a -> 'b t) -> 'b t
  val (>>|)  : 'a t -> ('a -> 'b)   -> 'b t
  val return : 'a -> 'a t
end

module Identity_monad = struct
  type 'a t = 'a
  let (>>=) x f = f x
  let (>>|) x f = f x
  let return x = x
end

module Make_monad(Keys : Keys)(M : Monad) = struct
  let top_closure ~key ~deps elements =
    let open M in
    let visited = ref Keys.empty in
    let res = ref [] in
    elements
    >>= fun elements ->
    let rec loop elt ~temporarily_marked =
      key elt
      >>= fun key ->
      if Keys.mem temporarily_marked key then
        return (Error [elt])
      else if not (Keys.mem !visited key) then begin
        visited := Keys.add !visited key;
        let temporarily_marked = Keys.add temporarily_marked key in
        deps elt
        >>= fun deps ->
        iter_elts deps ~temporarily_marked
        >>| fun result ->
        match result with
        | Ok () -> res := elt :: !res; Ok ()
        | Error l -> Error (elt :: l)
      end else
        return (Ok ())
    and iter_elts elts ~temporarily_marked =
      match elts with
      | [] -> return (Ok ())
      | elt :: elts ->
        loop elt ~temporarily_marked
        >>= fun result ->
        match result with
        | Error _ as result -> return result
        | Ok () -> iter_elts elts ~temporarily_marked
    in
    iter_elts elements ~temporarily_marked:Keys.empty
    >>| fun result ->
    match result with
    | Ok () -> Ok (List.rev !res)
    | Error elts -> Error elts
end

module Make(Keys : Keys) = struct
  module Id = struct
    include Make_monad (Keys) (Identity_monad)
  end

  module Fiber = struct
    include Make_monad (Keys) (struct
        include Fiber
        include Fiber.O
      end)
  end

  let top_closure = Id.top_closure

  let top_closure_f = Fiber.top_closure
end

module Int    = Make(Int.Set)
module String = Make(String.Set)
