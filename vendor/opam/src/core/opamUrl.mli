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

(** URL parsing and printing, with support for our different backends *)

type version_control = [ `git | `darcs | `hg ]

type backend = [ `http | `rsync | version_control ]

val string_of_backend: backend -> string

exception Parse_error of string

(** Tolerates lots of backward compatibility names;
    @raise Parse_error on unknown protocol *)
val backend_of_string: string -> [> backend]

type t = {
  transport: string; (** the part just before '://' *)
  path: string; (** the part after '://' *)
  hash: string option; (** the optional branch/ref specification,
                           at the end after a '#' *)
  backend: backend; (** the backend that opam should use to handle this
                        url *)
}

(** Same as [of_string], but allows enforcing the expected backend, and may
    otherwise guess version control from the suffix by default (for e.g.
    https://foo/bar.git). (this should be disabled when parsing from files).
    Note that [handle_suffix] also handles user-name in ssh addresses (e.g.
    "ssh://git@github.com/..."). If [from_file] is set to false, it resolves
    rsync/file relative path.
    @raise Parse_error *)
val parse:
  ?backend:backend -> ?handle_suffix:bool -> ?from_file:bool -> string -> t

(** Same as [parse], but catch [Parse_error]. In this case, display a warning
    if [quiet] is not set to true. *)
val parse_opt:
  ?quiet:bool -> ?backend:backend -> ?handle_suffix:bool -> ?from_file:bool
  -> string -> t option

include OpamStd.ABSTRACT with type t := t

(* [to_string_w_subpath subpath url] Return string of [url] with [subpath]
   integrated *)
val to_string_w_subpath: OpamFilename.SubPath.t option -> t -> string

(** Dummy filler url *)
val empty: t

(** Returns the url string without the VC part (i.e. "git+foo://bar" returns
    "foo://bar") *)
val base_url: t -> string

(** The last part of the url path, e.g. ["http://foo/bar/this"] or
    ["http://that.here/"] *)
val basename: t -> string

(** Returns the url with all path components but the first one (the hostname)
    dropped, e.g. ["http://some.host/some/path"] becomes ["http://some.host"] *)
val root: t -> t

val has_trailing_slash: t -> bool

(** Check if the URL matches an existing local directory, and return it *)
val local_dir: t -> OpamFilename.Dir.t option

(** Check if the URL matches an existing local file, and return it *)
val local_file: t -> OpamFilename.t option

(** If the given url-string has no 'transport://' specification and corresponds
    to an existing local path, check for version-control clues at that path *)
val guess_version_control: string -> [> version_control ] option

(** [map_file_url f url] applies [f] to the [path] portion of [url] if
    [transport] is ["file"]. *)
val map_file_url : (string -> string) -> t -> t

module Op: sig

  (** Appends at the end of an URL path with '/' separator. Gets back to the
      root if the second argument starts with '/' *)
  val ( / ) : t -> string -> t

end
