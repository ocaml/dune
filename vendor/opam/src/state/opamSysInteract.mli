(**************************************************************************)
(*                                                                        *)
(*    Copyright 2019-2020 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamStateTypes

(* Given a list of system packages, retrieve their installation status from the
   system and returns a pair of [sys_package] set:
     * first one is available set: package that exist on the default
       repositories, but not installed)
     * second one, not found set: packages not found on the defined repositories
   [env] is used to determine host specification. *)
val packages_status:
  ?env:gt_variables -> OpamFile.Config.t -> OpamSysPkg.Set.t ->
  OpamSysPkg.Set.t * OpamSysPkg.Set.t

(* Return the commands to run to install given system packages.
   [env] is used to determine host specification. *)
val install_packages_commands:
  ?env:gt_variables -> OpamFile.Config.t -> OpamSysPkg.Set.t ->
  ([`AsAdmin of string | `AsUser of string] * string list) list

(* Install given system packages, by calling local system package manager.
   [env] is used to determine host specification. *)
val install: ?env:gt_variables -> OpamFile.Config.t -> OpamSysPkg.Set.t -> unit

val update: ?env:gt_variables -> OpamFile.Config.t -> unit

val package_manager_name: ?env:gt_variables -> OpamFile.Config.t -> string

(* Determine if special packages may need installing to enable other
   repositories.
   Presently used to check for epel-release on CentOS and RHEL.
   [env] is used to determine host specification. *)
val repo_enablers: ?env:gt_variables -> OpamFile.Config.t -> string option


module Cygwin : sig

  (* Default Cygwin installation prefix C:\cygwin64 *)
  val default_cygroot: string

  (* Install an internal Cygwin install, in <root>/.cygwin *)
  val install: packages:OpamSysPkg.t list -> OpamFilename.t

  (* [check_install path] checks a Cygwin installation at [path]. It checks
     that 'path\cygcheck.exe' or 'path\bin\cygcheck.exe' exists. *)
  val check_install:
    string -> (OpamFilename.t, string) result

  (* Returns true if Cygwin install is internal *)
  val is_internal: OpamFile.Config.t -> bool

  (* [check_setup path] checks and store Cygwin setup executable. Is [path] is
     [None], it downloads it, otherwise it copies it to
     <opamroot>/.cygwin/setup-x86_64.exe. If the file is already existent, it
     is a no-op. *)
  val check_setup: OpamFilename.t option -> unit

  (* Return Cygwin binary path *)
  val cygbin_opt: OpamFile.Config.t -> OpamFilename.Dir.t option

  (* Return Cygwin cygcheck.exe path *)
  val cygcheck_opt: OpamFile.Config.t -> OpamFilename.t option

  (* Return Cygwin installation prefix *)
  val cygroot_opt: OpamFile.Config.t -> OpamFilename.Dir.t option

end
