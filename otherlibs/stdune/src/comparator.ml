module type S = sig
  type t

  val compare : t -> t -> Ordering.t
end

module type OPS = sig
  type t

  val equal : t -> t -> bool
  val ( = ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
end

module Operators (X : S) = struct
  type t = X.t

  let[@inline always] ( = ) a b =
    match X.compare a b with
    | Eq -> true
    | Gt | Lt -> false
  ;;

  let equal = ( = )
  let[@inline always] ( <> ) a b = not (a = b)

  let[@inline always] ( >= ) a b =
    match X.compare a b with
    | Gt | Eq -> true
    | Lt -> false
  ;;

  let[@inline always] ( > ) a b =
    match X.compare a b with
    | Gt -> true
    | Lt | Eq -> false
  ;;

  let[@inline always] ( <= ) a b =
    match X.compare a b with
    | Lt | Eq -> true
    | Gt -> false
  ;;

  let[@inline always] ( < ) a b =
    match X.compare a b with
    | Lt -> true
    | Gt | Eq -> false
  ;;
end
