(* $Id: findlib.mli,v 1.7 2002/04/26 15:45:22 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

val init : 
      ?env_ocamlpath: string ->
      ?env_ocamlfind_destdir: string ->
      ?env_ocamlfind_metadir: string ->
      ?env_ocamlfind_commands: string ->
      ?env_camllib: string ->
      ?env_ldconf: string ->
      ?config: string -> 
      unit ->
	unit
  (* Initializes the library from the configuration file and the environment. 
   * By default the
   * function reads the file specified at compile time, but you can also
   * pass a different file name in the ~config argument.
   *   Furthermore, the environment variables OCAMLPATH, OCAMLFIND_DESTDIR, 
   * OCAMLFIND_COMMANDS, and CAMLLIB are interpreted. By default, the function takes
   * the values found in the environment, but you can pass different values
   * using the ~env_* arguments. By setting these values to empty strings 
   * they are no longer considered.
   *     The result of the initialization is determined as follows:
   * - The default installation directory is the env variable OCAMLFIND_DESTDIR
   *   (if present and non-empty), and otherwise the variable "destdir" of the
   *   configuration file.
   * - The installation directory for META files is read from the env 
   *   variable OCAMLFIND_METADIR (if present and non-empty), and otherwise
   *   from the variable "metadir" of the configuration file, and otherwise
   *   no such directory is used.
   *   The special value "none" turns this feature off.
   * - The search path is the concatenation of the env variable OCAMLPATH
   *   and the variable "path" of the config file
   * - The executables of (ocamlc|ocamlopt|ocamlcp|ocamlmktop) are determined
   *   as follows: if the env variable OCAMLFIND_COMMANDS is set and non-empty,
   *   its contents specify the executables. Otherwise, if the config file
   *   variables "ocamlc", "ocamlopt", "ocamlcp" and "ocamlmktop" are set,
   *   their contents specify the executables. Otherwise, the obvious default
   *   values are chosen: "ocamlc" for "ocamlc", "ocamlopt" for "ocamlopt",
   *   and so on.
   * - The directory of the standard library is the value of the environment
   *   variable CAMLLIB (or OCAMLLIB), or if unset or empty, the value of
   *   the configuration variable "stdlib", or if unset the built-in location
   * - The "ld.conf" file (configuring the dynamic loader) is the value of
   *   the environment variable OCAMLFIND_LDCONF, or if unset or empty, the
   *   value of the configuration variable "ldconf", or if unset the
   *   built-in location.
   *)


val init_manually : 
      ?ocamlc_command: string ->       (* default: "ocamlc"     *)
      ?ocamlopt_command: string ->     (* default: "ocamlopt"   *)
      ?ocamlcp_command: string ->      (* default: "ocamlcp"    *)
      ?ocamlmktop_command: string ->   (* default: "ocamlmktop" *)
      ?ocamldep_command: string ->     (* default: "ocamldep"   *)
      ?ocamlbrowser_command: string -> (* default: "ocamlbrowser"   *)
      ?stdlib: string ->               (* default: taken from Findlib_config *)
      ?ldconf: string ->
      install_dir: string ->
      meta_dir: string ->
      search_path: string list ->
	unit
  (* This is an alternate way to initialize the library directly. 
   * Environment variables and configuration files are ignored.
   *)


val default_location : unit -> string
  (* Get the default installation directory for packages *)

val meta_directory : unit -> string
  (* Get the META installation directory for packages.
   * Returns "" if no such directory is configured.
   *)

val search_path : unit -> string list
  (* Get the search path for packages *)

val command : [ `ocamlc | `ocamlopt | `ocamlcp | `ocamlmktop | `ocamldep |
		`ocamlbrowser
	      ] -> 
              string
  (* Get the name/path of the executable *)

val ocaml_stdlib : unit -> string
  (* Get the directory of the standard library *)

val ocaml_ldconf : unit -> string
  (* Get the file name of "ld.conf" *)

val package_directory : string -> string
  (* get the absolute path of the directory where the given package is
   * stored.
   * raises Not_found if the package cannot be found.
   *)


val package_property : string list -> string -> string -> string
  (* package_property predlist pkg propname:
   * looks up the property 'propname' of package 'pkg' under the assumption
   * that the predicates in 'predlist' are true.
   * raises Not_found if the package or the property could not be found.
   *
   * EXAMPLES:
   * - package_property [] "p" "requires":
   *   get the value of the 'requires' clause of package 'p'
   * - package_property [ "mt"; "byte" ] "p" "archive":
   *   get the value of the 'archive' property of package 'p' for multi-
   *   threaded bytecode applications.
   *)

val package_ancestors : string list -> string -> string list
  (* package_ancestors predlist pkg:
   * determines the direct ancestors of package 'pkg' under the assumption
   * that the predicates in 'predlist' are true, i.e. the names of the
   * packages required by 'pkg'.
   * The returned list is unsorted.
   * raises Not_found if the package 'pkg' could not be found.
   * fails if one of the ancestors could not be found.
   *)

val package_deep_ancestors : string list -> string list -> string list
  (* package_deep_ancestors predlist pkglist:
   * determines the list of direct or indirect ancestors of the packages
   * named in 'pkglist' under the assumption that the predicates in 'predlist' 
   * are true. 
   * The returned list is topologically sorted: The first element is the
   * deepest ancestor; the last element is one of 'pkglist'.
   * raises Not_found if one of the packages in 'pkglist' could not be found.
   * fails if one of the ancestors could not be found.
   * fails if there is a cyclic dependency.
   *)


(* ======================================================================
 * History:
 *
 * $Log: findlib.mli,v $
 * Revision 1.7  2002/04/26 15:45:22  gerd
 * 	New: ocamlfind browser
 *
 * Revision 1.6  2002/04/23 23:25:03  gerd
 * 	New option -ldconf
 *
 * Revision 1.5  2001/09/04 16:11:27  gerd
 * 	Added command ocamldep.
 *
 * Revision 1.4  2001/07/24 19:57:00  gerd
 * 	New options for stdlib/camllib
 *
 * Revision 1.3  2001/02/24 20:19:15  gerd
 * 	Support for configuration file. New functions init, init_manually.
 *
 * Revision 1.2  1999/06/20 19:26:22  gerd
 * 	Major change: Added support for META files. In META files, knowledge
 * about compilation options, and dependencies on other packages can be stored.
 * The "ocamlfind query" subcommand has been extended in order to have a
 * direct interface for that. "ocamlfind ocamlc/ocamlopt/ocamlmktop/ocamlcp"
 * subcommands have been added to simplify the invocation of the compiler.
 *
 * Revision 1.1  1999/03/26 00:02:47  gerd
 * 	Initial release.
 *
 *
 *)
