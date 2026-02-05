include module type of Dune_lang.Package_name

val of_opam_package_name : OpamTypes.name -> t
val to_opam_package_name : t -> OpamPackage.Name.t
