(** Support for [pin-depends] *)

(** Collect and resolve all pins in [local_packages] using the sources defined
    in [local_packages].

    Pins are defined using the [pin-depends] field of opam files. We traverse
    all local packages to collect such fields and then fetch and return all the
    opam files they correspond to *)

val collect : Local_package.t Package_name.Map.t -> Local_package.pin Package_name.Map.t
val resolve_package : Local_package.pin -> Resolved_package.t Fiber.t
