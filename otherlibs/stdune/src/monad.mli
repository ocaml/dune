(** Monad signatures *)

module type Basic = Monad_intf.Basic
module type S = Monad_intf.S
module type List = Monad_intf.List
module type Option = Monad_intf.Option
module type Result = Monad_intf.Result

module Make (M : Basic) : S with type 'a t := 'a M.t
module Id : S with type 'a t = 'a
module List (M : S) : List with type 'a t := 'a M.t
module Option (M : S) : Option with type 'a t := 'a M.t
module Result (M : S) : Result with type 'a t := 'a M.t
