(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018-2020 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module String = struct
  [@@@warning "-32"]

  (** NOTE: OCaml >= 4.13 *)
  let exists p s =
    let n = String.length s in
    let rec loop i =
      if i = n then false
      else if p (String.unsafe_get s i) then true
      else loop (succ i) in
    loop 0

  include Stdlib.String
end

module Either = struct
  (** NOTE: OCaml >= 4.12 *)
  type ('a, 'b) t =
    | Left of 'a
    | Right of 'b
end

module Unix = struct
  [@@@warning "-32"]

  (** NOTE: OCaml >= 4.13 *)
  let realpath s =
    let getchdir s =
      let p =
        try Sys.getcwd ()
        with Sys_error _ -> Filename.get_temp_dir_name ()
      in
      Unix.chdir s;
      p
    in
    try getchdir (getchdir s) with Unix.Unix_error _ -> s

  include Unix
end

module Lazy = struct
  [@@@warning "-32"]

  (** NOTE: OCaml >= 4.13 *)
  let map f x =
    lazy (f (Lazy.force x))

  include Stdlib.Lazy
end
