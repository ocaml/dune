(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* Written by: Emilio Jesús Gallego Arias      *)

open! Stdune

(* At some point we may want to reify this into resolved status, for example:

   ; libraries : Lib.t list Or_exn.t

   etc.. *)
type t =
  { name : Loc.t * Coq_lib_name.t
  ; wrapper : string
  ; src_root : Path.Build.t
  ; obj_root : Path.Build.t
  ; theories : (Loc.t * Coq_lib_name.t) list
  ; libraries : (Loc.t * Lib_name.t) list
  ; package : Package.t option
  }

let name l = snd l.name

let wrapper l = l.wrapper

let src_root l = l.src_root

let obj_root l = l.obj_root

let libraries l = l.libraries

let package l = l.package

module DB = struct
  type lib = t

  type nonrec t =
    { boot : (Loc.t * t) option
    ; libs : t Coq_lib_name.Map.t
    }

  let boot_library { boot; _ } = boot

  let create_from_stanza ((dir, s) : Path.Build.t * Dune_file.Coq.t) =
    let name = snd s.name in
    ( name
    , { name = s.name
      ; wrapper = Coq_lib_name.wrapper name
      ; obj_root = dir
      ; src_root = dir
      ; theories = s.theories
      ; libraries = s.libraries
      ; package = s.package
      } )

  (* Should we register errors and printers, or raise is OK? *)
  let create_from_coqlib_stanzas sl =
    let libs =
      match Coq_lib_name.Map.of_list_map ~f:create_from_stanza sl with
      | Ok m -> m
      | Error (name, _w1, w2) ->
        let loc = (snd w2).loc in
        User_error.raise ~loc
          [ Pp.textf "Duplicate theory name: %s" (Coq_lib_name.to_string name) ]
    in
    let boot =
      match List.find_all ~f:(fun (_, s) -> s.Dune_file.Coq.boot) sl with
      | [] -> None
      | [ l ] -> Some ((snd l).loc, snd (create_from_stanza l))
      | _ :: (_, s2) :: _ ->
        User_error.raise ~loc:s2.loc
          [ Pp.textf "Cannot have more than one boot library: %s)"
              (Coq_lib_name.to_string (snd s2.name))
          ]
    in
    { boot; libs }

  let resolve db (loc, name) =
    match Coq_lib_name.Map.find db.libs name with
    | None ->
      Error
        User_error.(
          E
            (make ~loc
               [ Pp.textf "Theory %s not found" (Coq_lib_name.to_string name) ]))
    | Some s -> Ok s

  let find_many t ~loc = Result.List.map ~f:(fun name -> resolve t (loc, name))

  (* Where should we move this? *)
  module Result_monad : Monad_intf.S with type 'a t = 'a Or_exn.t =
  struct
    type 'a t = 'a Or_exn.t

    let return x = Ok x

    let ( >>= ) = Result.O.( >>= )
  end

  module Coq_lib_closure = Top_closure.Make (String.Set) (Result_monad)

  let requires db t : lib list Or_exn.t =
    let theories =
      match db.boot with
      | None -> t.theories
      (* XXX: Note that this means that we will prefix Coq with -Q, not sure we
         want to do that (yet), but seems like good practice. *)
      | Some (loc, stdlib) -> (loc, snd stdlib.name) :: t.theories
    in
    let open Result.O in
    let* theories = Result.List.map ~f:(resolve db) theories in
    let key t = Coq_lib_name.to_string (snd t.name) in
    let deps t = Result.List.map ~f:(resolve db) t.theories in
    Result.bind (Coq_lib_closure.top_closure theories ~key ~deps) ~f:(function
      | Ok libs -> Ok libs
      | Error cycle ->
        let msg =
          [ Pp.textf "Cycle found"
          ; Pp.enumerate cycle ~f:(fun t ->
                Pp.text (Coq_lib_name.to_string (snd t.name)))
          ]
        in
        Error User_error.(E (make ~loc:(fst t.name) msg)))
end
