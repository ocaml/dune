open! Import

type t

val true_ : t
val false_ : t

(** Construct a value of type string *)
val string : string -> t

val equal : t -> t -> bool
val hash : t -> int
val digest_feed : t Dune_digest.Feed.t
val compare : t -> t -> ordering
val to_dyn : t -> Dyn.t
val decode : t Decoder.t
val encode : t Encoder.t
val to_string : t -> string
val to_opam_filter : t -> OpamTypes.filter
val to_opam_variable_contents : t -> OpamTypes.variable_contents

(** [sentinel_value_of_variable_name name] is a value based on [name] that is
    very unlikely to be seen in the wild as a value of the given variable. A
    variable may be assigned a sentinel value in situations where it would
    otherwise be undefined, and when an undefined value would lead to
    undesirable consequences.

    For example the "os-version" opam variable rarely has an impact on solving
    in practice, however some package declare that they are unavailable for old
    versions of particular operating systems distros, such as:

      available: (os-distribution != "ubuntu" | os-version >= "18.04")

    This states that the package is unavailable on versions of ubuntu earlier
    than 18.04. Undefined values in opam are propagated through operators, so
    if os-version (or os-distribution) were unset, the entire availability
    condition would evaluate to an undefined value, which is coerced into
    "false". That is to say that this package would be considered unavailable
    unless os-version and os-distribution have values. Setting these variables
    to sentinel values is sufficient for this package to be considered
    available.

    The value is a string made up of the variable's name converted to all caps,
    with dashes replaced with underscores and prepended with "__". For example
    "os-version" would become "__OS_VERSION". *)
val sentinel_value_of_variable_name : Package_variable_name.t -> t
