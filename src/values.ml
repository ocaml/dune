open! Import

type 'a t =
  | []     : unit t
  | ( :: ) : 'a * 'b t -> ('a -> 'b) t

module Spec = struct
  type 'a t =
    | []     : unit t
    | ( :: ) : (string * 'a Kind.t) * 'b t -> ('a -> 'b) t

  let rec filenames : type a. a t -> string list = function
    | [] -> []
    | (fn, _) :: t -> fn :: filenames t
end
