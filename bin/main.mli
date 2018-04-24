type which = Dune | Jbuilder

module Make(M : sig val which : which end) : sig end
