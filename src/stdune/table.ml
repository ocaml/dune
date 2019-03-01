module type Hashtbl = sig
  module Key : sig type t val equal : t -> t -> bool val hash : t -> int end
  module H : module type of Hashtbl.Make(Key)

  type value
  val value : value H.t
end

type ('input, 'output) t =
  (module Hashtbl with type value = 'output and type Key.t = 'input)

module type Key = sig type t val equal : t -> t -> bool val hash : t -> int end

let create (type key) (type value)
      (module Key : Key with type t = key) size : (key, value) t =
  (module struct
    module Key = Key
    module H = Hashtbl.Make(Key)
    type nonrec value = value
    let value = H.create size
  end)

let find (type input) (type output) ((module T) : (input, output) t) x =
  T.H.find T.value x

let add (type input) (type output) ((module T) : (input, output) t) k v =
  T.H.add T.value k v

let clear (type input) (type output) ((module T) : (input, output) t) =
  T.H.clear T.value
