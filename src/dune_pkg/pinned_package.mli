(** Support for [pin-depends] *)

(** Collect and resolve all pins in [local_packages] using the sources defined
    in [local_packages].

    Pins are defined using the [pin-depends] field of opam files. We traverse
    all local packages to collect such fields and then fetch and return all the
    opam files they correspond to *)
val resolve_pins
  :  Local_package.For_solver.t Package_name.Map.t
  -> Resolved_package.t Package_name.Map.t Fiber.t
