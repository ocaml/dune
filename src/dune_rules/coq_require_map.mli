module Path := Coq_module.Path
module Name := Coq_module.Name

type 'a t

val empty : 'a t

val add : 'a t -> Path.t -> 'a -> 'a t

val of_modules : Coq_module.t list -> Coq_module.t t

val merge_all : 'a t list -> 'a t

val find_all :
  Coq_module.t t -> prefix:Path.t -> suffix:Path.t -> Coq_module.t list

val equal : Coq_module.t t -> Coq_module.t t -> bool

val to_dyn : Coq_module.t t -> Dyn.t
