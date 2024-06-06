val do_something : unit -> unit
val fromotherlib : int


include (module type of Imp_lib)
type u = Imp_lib.t
