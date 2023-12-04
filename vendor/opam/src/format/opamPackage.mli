(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** The package type, and package name type (name+version, values often called
    "nv" in the code) *)

(** {2 Package name and versions} *)

(** Versions *)
module Version: sig

  include OpamStd.ABSTRACT

  (** Compare two versions using the Debian version scheme *)
  val compare: t -> t -> int

  (** Are two package versions equal? *)
  val equal: t -> t -> bool

  (** Default version used when no version is given *)
  val default : t
end

(** Names *)
module Name: sig
  include OpamStd.ABSTRACT

  (** Compare two package names *)
  val compare: t -> t -> int

  (** Are two package names equal? *)
  val equal: t -> t -> bool
end

type t = private {
  name: Name.t;
  version: Version.t;
}

(** Package (name x version) pairs *)
include OpamStd.ABSTRACT with type t := t

(** Return the package name *)
val name: t -> Name.t

(** Return None if [nv] is not a valid package name *)
val of_string_opt: string -> t option

(** Return the version name *)
val version: t -> Version.t

(** Create a new pair (name x version) *)
val create: Name.t -> Version.t -> t

(** To fit in the GenericPackage type, for generic display functions *)
val name_to_string: t -> string
val version_to_string: t -> string

(** Guess the package name from a filename. This function extracts
    [name] and [version] from {i /path/to/$name.$version/opam}, or
    {i /path/to/$name.$version.opam} *)
val of_filename: OpamFilename.t -> t option

(** Guess the package name from a directory name. This function extracts {i
    $name} and {i $version} from {i /path/to/$name.$version/} *)
val of_dirname: OpamFilename.Dir.t -> t option

(** Guess the package name from an archive file. This function extract
    {i $name} and {i $version} from {i
    /path/to/$name.$version+opam.tar.gz} *)
val of_archive: OpamFilename.t -> t option

(** Convert a set of pairs to a map [name -> versions] *)
val to_map: Set.t -> Version.Set.t Name.Map.t

(** The converse of [to_map] *)
val of_map: Version.Set.t Name.Map.t -> Set.t

(** Returns the keys in a package map as a package set *)
val keys: 'a Map.t -> Set.t

(** Extract the versions from a collection of packages *)
val versions_of_packages: Set.t -> Version.Set.t

(** Return the list of versions for a given package *)
val versions_of_name: Set.t -> Name.t -> Version.Set.t

(** Extract the naes from a collection of packages *)
val names_of_packages: Set.t -> Name.Set.t

(** Returns true if the set contains a package with the given name *)
val has_name: Set.t -> Name.t -> bool

(** Return all the packages with the given name *)
val packages_of_name: Set.t -> Name.t -> Set.t
val packages_of_name_map: 'a Map.t -> Name.t -> 'a Map.t

(** Return a package with the given name *)
val package_of_name: Set.t -> Name.t -> t

(** Return a package with the given name, if any *)
val package_of_name_opt: Set.t -> Name.t -> t option

(** Return all the packages with one of the given names *)
val packages_of_names: Set.t -> Name.Set.t -> Set.t

(** Removes all packages with the given name from a set of packages *)
val filter_name_out: Set.t -> Name.t -> Set.t

(** Return the maximal available version of a package name from a set.
    Raises [Not_found] if no such package available. *)
val max_version: Set.t -> Name.t -> t

(** Compare two packages *)
val compare: t -> t -> int

(** Are two packages equal? *)
val equal: t -> t -> bool

(** Hash a package *)
val hash: t -> int

(** Return all the package descriptions in a given directory *)
val list: OpamFilename.Dir.t -> Set.t

(** Return all the package descriptions in the current directory (and
    their eventual prefixes). *)
val prefixes: OpamFilename.Dir.t -> string option Map.t

(** {2 Errors} *)
