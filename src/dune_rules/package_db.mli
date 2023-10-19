open Import

type t

type any_package =
  | Local of Package.t
  | Installed of Dune_package.t
  | Build of unit Action_builder.t

val create : Context_name.t -> t Memo.t
val find_package : t -> Package.Name.t -> any_package option Memo.t

val section_of_any_package_site
  :  any_package
  -> Package.Name.t
  -> Loc.t
  -> Site.t
  -> Dune_section.t

val section_of_site
  :  t
  -> loc:Loc.t
  -> pkg:Package.Name.t
  -> site:Site.t
  -> Section.t Memo.t
