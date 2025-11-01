open Stdune
open Fiber.O

module type Implicit_output = sig
  type t

  val name : string
  val union : t -> t -> t
end

module Witness : sig
  type 'a t

  val create : (module Implicit_output with type t = 'a) -> 'a t

  (* val get : 'a t -> (module Implicit_output with type t = 'a) *)

  val get_name : 'a t -> string
  val get_union : 'a t -> 'a -> 'a -> 'a
  val same : 'a t -> 'b t -> ('a, 'b) Type_eq.t option
end = struct
  type _ w = ..

  module type T = sig
    type a
    type _ w += W : a w

    include Implicit_output with type t = a
  end

  type 'a t = (module T with type a = 'a)

  let create (type a) (module I : Implicit_output with type t = a) : a t =
    (module struct
      type nonrec a = a
      type _ w += W : a w

      include I
    end)
  ;;

  let get (type a) (module T : T with type a = a) =
    (module T : Implicit_output with type t = a)
  ;;

  let get_name (type a) t =
    let module I = (val get t : Implicit_output with type t = a) in
    I.name
  ;;

  let get_union (type a) t =
    let module I = (val get t : Implicit_output with type t = a) in
    I.union
  ;;

  let same (type a b) ((module M1) : a t) ((module M2) : b t) =
    match M1.W with
    | M2.W -> Some (Type_eq.T : (a, b) Type_eq.t)
    | _ -> None
  ;;
end

type 'o t = 'o Witness.t

module type Handler = sig
  type o

  val type_ : o t
  val so_far : o option ref
end

type handler = (module Handler)

let current_handler : handler option Fiber.Var.t = Fiber.Var.create None

let produce' ~union opt v =
  match !opt with
  | None -> opt := Some v
  | Some v0 -> opt := Some (union v0 v)
;;

let produce (type o) (type_ : o t) (value : o) =
  let+ current_handler = Fiber.Var.get current_handler in
  match current_handler with
  | None ->
    Code_error.raise
      "Implicit_output.produce called without any handler in dynamic scope"
      [ "type", String (Witness.get_name type_) ]
  | Some (module H : Handler) ->
    (match Witness.same type_ H.type_ with
     | Some Type_eq.T -> produce' ~union:(Witness.get_union type_) H.so_far value
     | None ->
       Code_error.raise
         "Implicit_output.produce called with a handler for a different output"
         [ "type_handled", String (Witness.get_name H.type_)
         ; "type_produced", String (Witness.get_name type_)
         ])
;;

let produce_opt t v =
  match v with
  | None -> Fiber.return ()
  | Some v -> produce t v
;;

let collect (type o) (type_ : o t) f =
  (* If we don't delay the computation here, [output] becomes shared between
     future runs of the resulting fiber, causing the collected output to be
     duplicated. *)
  Fiber.of_thunk (fun () ->
    let output = ref None in
    Fiber.map
      (Fiber.Var.set
         current_handler
         (Some
            (module struct
              type nonrec o = o

              let type_ = type_
              let so_far = output
            end))
         f)
      ~f:(fun res -> res, !output))
;;

let forbid f = Fiber.Var.set current_handler None f
let add (type a) (module T : Implicit_output with type t = a) = Witness.create (module T)
