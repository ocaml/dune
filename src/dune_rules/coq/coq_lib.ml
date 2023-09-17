open Import

(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2020                              *)
(* Written by: Emilio Jesús Gallego Arias *)

module Id = struct
  module T = struct
    type t =
      { path : Path.t
      ; loc : Loc.t
      ; name : Coq_lib_name.t
      }

    let compare t { path; name; _ } =
      let open Ordering.O in
      let= () = Path.compare t.path path in
      Coq_lib_name.compare t.name name
    ;;

    let to_dyn { path; loc; name } =
      Dyn.(
        record
          [ "path", Path.to_dyn path
          ; "loc", Loc.to_dyn loc
          ; "name", Coq_lib_name.to_dyn name
          ])
    ;;
  end

  include T

  let pp { path; loc; name } =
    let loc_or_path =
      if Loc.is_none loc then Path.pp path else Loc.pp_file_colon_line loc
    in
    Pp.concat
      ~sep:Pp.space
      [ Pp.textf "theory %s in" (Coq_lib_name.to_string name); loc_or_path ]
  ;;

  let create ~path ~name:(loc, name) = { path; name; loc }

  module C = Comparable.Make (T)
  module Top_closure = Top_closure.Make (C.Set) (Resolve)

  let top_closure ~key ~deps xs = Top_closure.top_closure ~key ~deps xs
end

module rec R : sig
  type t =
    | Dune of Dune.t
    | Legacy of Legacy.t

  val to_dyn : t -> Dyn.t
end = struct
  type t =
    | Dune of Dune.t
    | Legacy of Legacy.t

  let to_dyn = function
    | Dune t -> Dyn.Variant ("Dune", [ Dune.to_dyn t ])
    | Legacy t -> Dyn.Variant ("Legacy", [ Legacy.to_dyn t ])
  ;;
end

and Dune : sig
  type t =
    { loc : Loc.t
    ; boot_id : Id.t option (** boot library that was selected to build this theory *)
    ; id : Id.t
    ; implicit : bool (* Only useful for the stdlib *)
    ; use_stdlib : bool
        (* whether this theory uses the stdlib, eventually set to false for all libs *)
    ; src_root : Path.Build.t
    ; obj_root : Path.Build.t
    ; theories : (Loc.t * R.t) list Resolve.t
    ; libraries : (Loc.t * Lib.t) list Resolve.t
    ; theories_closure : R.t list Resolve.t Lazy.t
    ; package : Package.t option
    }

  val to_dyn : t -> Dyn.t
  val src_root : t -> Path.Build.t
  val obj_root : t -> Path.Build.t
  val implicit : t -> bool

  (** ML libraries *)
  val libraries : t -> (Loc.t * Lib.t) list Resolve.t
end = struct
  type t =
    { loc : Loc.t
    ; boot_id : Id.t option (** boot library that was selected to build this theory *)
    ; id : Id.t
    ; implicit : bool (* Only useful for the stdlib *)
    ; use_stdlib : bool
        (* whether this theory uses the stdlib, eventually set to false for all libs *)
    ; src_root : Path.Build.t
    ; obj_root : Path.Build.t
    ; theories : (Loc.t * R.t) list Resolve.t
    ; libraries : (Loc.t * Lib.t) list Resolve.t
    ; theories_closure : R.t list Resolve.t Lazy.t
    ; package : Package.t option
    }

  let to_dyn
    { loc
    ; boot_id
    ; id
    ; implicit
    ; use_stdlib
    ; src_root
    ; obj_root
    ; theories
    ; libraries
    ; theories_closure
    ; package
    }
    =
    Dyn.(
      record
        [ "loc", Loc.to_dyn loc
        ; "boot_id", Dyn.option Id.to_dyn boot_id
        ; "id", Id.to_dyn id
        ; "implicit", Bool.to_dyn implicit
        ; "use_stdlib", Bool.to_dyn use_stdlib
        ; "src_root", Path.Build.to_dyn src_root
        ; "obj_root", Path.Build.to_dyn obj_root
        ; "theories", Resolve.to_dyn (Dyn.list (Dyn.pair Loc.to_dyn R.to_dyn)) theories
        ; ( "libraries"
          , Resolve.to_dyn (Dyn.list (Dyn.pair Loc.to_dyn Lib.to_dyn)) libraries )
        ; ( "theories_closure"
          , Resolve.to_dyn (Dyn.list R.to_dyn) (Lazy.force theories_closure) )
        ; "package", Dyn.option Package.to_dyn package
        ])
  ;;

  let src_root t = t.src_root
  let obj_root t = t.obj_root
  let implicit t = t.implicit
  let libraries t = t.libraries
end

and Legacy : sig
  type t =
    { boot_id : Id.t option
    ; id : Id.t
    ; implicit : bool (* Only useful for the stdlib *)
    ; installed_root : Path.t (* ; libraries : (Loc.t * Lib.t) list Resolve.t *)
    ; vo : Path.t list
    ; cmxs_directories : Path.t list
    }

  val to_dyn : t -> Dyn.t
  val implicit : t -> bool
  val installed_root : t -> Path.t
  val cmxs_directories : t -> Path.t list
  val vo : t -> Path.t list
end = struct
  type t =
    { boot_id : Id.t option
    ; id : Id.t
    ; implicit : bool (* Only useful for the stdlib *)
    ; installed_root : Path.t (* ; libraries : (Loc.t * Lib.t) list Resolve.t *)
    ; vo : Path.t list
    ; cmxs_directories : Path.t list
    }

  let to_dyn { boot_id; id; implicit; installed_root; vo; cmxs_directories } =
    Dyn.record
      [ "boot_id", Dyn.option Id.to_dyn boot_id
      ; "id", Id.to_dyn id
      ; "implicit", Dyn.bool implicit
      ; "installed_root", Path.to_dyn installed_root
      ; "vo", Dyn.list Path.to_dyn vo
      ; "cmxs_directories", Dyn.list Path.to_dyn cmxs_directories
      ]
  ;;

  let implicit t = t.implicit
  let installed_root t = t.installed_root
  let cmxs_directories t = t.cmxs_directories
  let vo t = t.vo
end

include R

let boot_id_of_lib = function
  | Dune t -> t.boot_id
  | Legacy t -> t.boot_id
;;

let id_of_lib = function
  | Dune t -> t.id
  | Legacy t -> t.id
;;

let name = function
  | Dune t -> t.id.name
  | Legacy t -> t.id.name
;;

let obj_root = function
  | Dune t -> Dune.obj_root t |> Path.build
  | Legacy t -> Legacy.installed_root t
;;

let implicit = function
  | Dune t -> Dune.implicit t
  | Legacy t -> Legacy.implicit t
;;

module Error = struct
  let annots = User_message.Annots.singleton User_message.Annots.needs_stack_trace ()

  let duplicate_theory_name (id1 : Id.t) (id2 : Id.t) =
    let name = id1.name in
    User_error.raise
      [ Pp.textf "Coq theory %s is defined twice:" (Coq_lib_name.to_string name)
      ; Pp.enumerate ~f:Id.pp [ id1; id2 ]
      ]
  ;;

  let incompatible_boot id1 id2 =
    User_message.make
      ~annots
      [ Pp.textf "The following theories use incompatible boot libraries:"
      ; Pp.enumerate ~f:Id.pp [ id1; id2 ]
      ]
    |> Resolve.fail
  ;;

  let theory_not_found ~loc ?hints name =
    let name = Coq_lib_name.to_string name in
    Resolve.Memo.fail
    @@ User_message.make
         ~annots
         ~loc
         ?hints
         [ Pp.textf "Theory %S has not been found." name ]
  ;;

  module Hidden_reason = struct
    type t =
      | LackOfTheoryComposition
      | LackOfInstalledComposition
  end

  let hidden_without_composition ~loc ~reason name =
    let name = Coq_lib_name.to_string name in
    let reason =
      match reason with
      | Hidden_reason.LackOfTheoryComposition ->
        Pp.textf
          "Upgrade coq lang to 0.4 to enable composition with theories in a different \
           scope."
      | LackOfInstalledComposition ->
        Pp.textf "Upgrade coq lang to 0.8 to enable composition with installed theories."
    in
    Resolve.Memo.fail
    @@ User_message.make
         ~annots
         ~loc
         [ Pp.textf "Theory %s not found in the current scope." name; reason ]
  ;;

  let private_deps_not_allowed ~loc name =
    let name = Coq_lib_name.to_string name in
    Resolve.Memo.fail
    @@ User_message.make
         ~loc
         [ Pp.textf
             "Theory %S is private, it cannot be a dependency of a public theory. You \
              need to associate %S to a package."
             name
             name
         ]
  ;;

  let duplicate_boot_lib theories =
    let open Coq_stanza.Theory in
    let name (t : Coq_stanza.Theory.t) =
      let name = Coq_lib_name.to_string (snd t.name) in
      Pp.textf "%s at %s" name (Loc.to_file_colon_line t.buildable.loc)
    in
    User_error.raise
      [ Pp.textf "Cannot have more than one boot theory in scope:"
      ; Pp.enumerate theories ~f:name
      ]
  ;;

  (* XXX: Not in the test suite *)
  let duplicate_boot_path paths =
    let describe_path cp =
      let name = Coq_path.name cp in
      let path = Coq_path.path cp in
      Pp.textf "%s at %s" (Coq_lib_name.to_string name) (Path.to_string path)
    in
    User_error.raise
      [ Pp.textf "Found more than one installed Coq standard libraries"
      ; Pp.enumerate paths ~f:describe_path
      ]
  ;;
end

let top_closure =
  let key t = id_of_lib t in
  let deps t =
    match t with
    | Dune t -> t.theories |> Resolve.map ~f:(List.map ~f:snd)
    | Legacy _ -> Resolve.return []
  in
  fun theories ->
    let open Resolve.O in
    Id.top_closure theories ~key ~deps
    >>= function
    | Ok s -> Resolve.return s
    | Error _ -> assert false
;;

module DB = struct
  type lib = t

  module Resolve_result = struct
    (** This is the first result of resolving a coq library, later one we will
        refine this data. *)
    type 'a t =
      | Redirect of 'a
      | Theory of Lib.DB.t * Path.Build.t * Coq_stanza.Theory.t
      | Legacy_theory of Coq_path.t
      | Not_found
  end

  type t =
    { parent : t option
    ; resolve : Coq_lib_name.t -> t Resolve_result.t
    ; boot_id : Id.t option
    }

  let rec to_dyn { parent; resolve = _; boot_id } =
    Dyn.record
      [ "parent", Dyn.option to_dyn parent; "boot_id", Dyn.option Id.to_dyn boot_id ]
  ;;

  module Entry = struct
    type nonrec t =
      | Redirect of t
      | Theory of Path.Build.t
  end

  let rec boot_library_id coq_db =
    match coq_db.boot_id with
    | Some boot_id -> Some boot_id
    | None ->
      (match coq_db.parent with
       | None -> None
       | Some parent -> boot_library_id parent)
  ;;

  module rec R : sig
    val resolve_boot
      :  t
      -> coq_lang_version:Dune_sexp.Syntax.Version.t
      -> (Loc.t * lib) option Resolve.Memo.t

    val resolve
      :  t
      -> coq_lang_version:Dune_sexp.Syntax.Version.t
      -> Loc.t * Coq_lib_name.t
      -> lib Resolve.Memo.t
  end = struct
    open R

    let resolve_plugin ~db ~allow_private_deps ~name (loc, lib) =
      let open Resolve.Memo.O in
      let* lib = Lib.DB.resolve db (loc, lib) in
      let+ () =
        Resolve.Memo.lift
        @@
        if allow_private_deps
        then Resolve.return ()
        else (
          match
            let info = Lib.info lib in
            let status = Lib_info.status info in
            Lib_info.Status.is_private status
          with
          | false -> Resolve.return ()
          | true ->
            Resolve.fail
            @@ User_message.make
                 ~loc
                 [ Pp.textf
                     "private theory %s may not depend on a public library"
                     (Coq_lib_name.to_string name)
                 ])
      in
      loc, lib
    ;;

    let resolve_plugins ~db ~allow_private_deps ~name plugins =
      let f = resolve_plugin ~db ~allow_private_deps ~name in
      Resolve.Memo.List.map plugins ~f
    ;;

    let check_boot ~boot_id (lib : lib) =
      match boot_id, boot_id_of_lib lib with
      | Some id, Some id' ->
        if Id.compare id id' = Eq
        then Resolve.return ()
        else Error.incompatible_boot id id'
      | _, _ -> Resolve.return ()
    ;;

    let maybe_add_boot ~boot ~use_stdlib ~is_boot theories =
      if use_stdlib && not is_boot
      then
        let open Resolve.O in
        let* boot = boot in
        let+ theories = theories in
        Option.to_list boot @ theories
      else theories
    ;;

    let resolve_boot ~coq_lang_version ~coq_db (boot_id : Id.t option) =
      match boot_id with
      | Some boot_id ->
        let open Resolve.Memo.O in
        let+ lib = resolve ~coq_lang_version coq_db (boot_id.loc, boot_id.name) in
        Some (boot_id.loc, lib)
      | None -> Resolve.Memo.return None
    ;;

    let resolve_theory
      ~coq_lang_version
      ~allow_private_deps
      ~coq_db
      ~boot_id
      (loc, theory_name)
      =
      let open Resolve.Memo.O in
      let* theory = resolve ~coq_lang_version coq_db (loc, theory_name) in
      let* () = Resolve.Memo.lift @@ check_boot ~boot_id theory in
      let+ () =
        if allow_private_deps
        then Resolve.Memo.return ()
        else (
          match theory with
          | Dune { package = None; _ } -> Error.private_deps_not_allowed ~loc theory_name
          | Legacy _ | Dune _ -> Resolve.Memo.return ())
      in
      loc, theory
    ;;

    let resolve_theories ~coq_lang_version ~allow_private_deps ~coq_db ~boot_id theories =
      let f = resolve_theory ~coq_lang_version ~allow_private_deps ~coq_db ~boot_id in
      Resolve.Memo.List.map theories ~f
    ;;

    let create_from_stanza_impl (coq_db, db, dir, (s : Coq_stanza.Theory.t)) =
      let name = s.name in
      let id = Id.create ~path:(Path.build dir) ~name in
      let coq_lang_version = s.buildable.coq_lang_version in
      let open Memo.O in
      let boot_id = if s.boot then None else boot_library_id coq_db in
      let allow_private_deps = Option.is_none s.package in
      let use_stdlib = s.buildable.use_stdlib in
      let+ libraries =
        resolve_plugins ~db ~allow_private_deps ~name:(snd name) s.buildable.plugins
      and+ theories =
        resolve_theories
          ~coq_db
          ~coq_lang_version
          ~allow_private_deps
          ~boot_id
          s.buildable.theories
      and+ boot = resolve_boot ~coq_lang_version ~coq_db boot_id in
      let theories = maybe_add_boot ~boot ~use_stdlib ~is_boot:s.boot theories in
      let map_error x =
        let human_readable_description () = Id.pp id in
        Resolve.push_stack_frame ~human_readable_description x
      in
      let theories = map_error theories in
      let libraries = map_error libraries in
      { Dune.loc = s.buildable.loc
      ; boot_id
      ; id
      ; use_stdlib
      ; implicit = s.boot
      ; obj_root = dir
      ; src_root = dir
      ; theories
      ; libraries
      ; theories_closure =
          lazy
            (Resolve.bind theories ~f:(fun theories ->
               List.map theories ~f:snd |> top_closure))
      ; package = s.package
      }
    ;;

    module Input = struct
      type nonrec t = t * Lib.DB.t * Path.Build.t * Coq_stanza.Theory.t

      let equal (coq_db, ml_db, path, stanza) (coq_db', ml_db', path', stanza') =
        phys_equal coq_db coq_db'
        && phys_equal ml_db ml_db'
        && Path.Build.equal path path'
        && phys_equal stanza stanza'
      ;;

      let hash = Poly.hash
      let to_dyn = Dyn.opaque
    end

    let memo =
      Memo.create
        "create-from-stanza"
        ~human_readable_description:(fun (_, _, path, theory) ->
          Id.pp (Id.create ~path:(Path.build path) ~name:theory.name))
        ~input:(module Input)
        create_from_stanza_impl
    ;;

    let create_from_stanza coq_db db dir stanza = Memo.exec memo (coq_db, db, dir, stanza)

    (* XXX: Memoize? This is pretty cheap so not sure worth the cost,
       still called too much I have observed, suspicious! *)
    let create_from_coqpath ~boot_id cp =
      let name = Coq_path.name cp in
      let installed_root = Coq_path.path cp in
      let implicit = Coq_path.stdlib cp in
      let cmxs_directories = Coq_path.cmxs_directories cp in
      let vo = Coq_path.vo cp in
      let id = Id.create ~path:installed_root ~name:(Loc.none, name) in
      Resolve.Memo.return
        { Legacy.boot_id; id; implicit; installed_root; vo; cmxs_directories }
    ;;

    module Resolve_result_no_redirect = struct
      (** In our second iteration, we remove all the redirects *)
      type t =
        | Theory of Lib.db * Path.Build.t * Coq_stanza.Theory.t
        | Legacy_theory of Coq_path.t
        | Not_found
    end

    let rec find coq_db name : Resolve_result_no_redirect.t =
      match coq_db.resolve name with
      | Theory (db, dir, stanza) -> Theory (db, dir, stanza)
      | Legacy_theory cp -> Legacy_theory cp
      | Redirect coq_db -> find coq_db name
      | Not_found ->
        (match coq_db.parent with
         | None -> Not_found
         | Some parent -> find parent name)
    ;;

    module Resolve_final_result = struct
      (** Next we find corresponding Coq libraries for the various cases *)
      type t =
        | Found_stanza of Lib.DB.t * Path.Build.t * Coq_stanza.Theory.t
        | Found_path of Coq_path.t
        | Hidden of Error.Hidden_reason.t
        | Not_found
    end

    let find coq_db ~coq_lang_version name : Resolve_final_result.t =
      match find coq_db name with
      | Not_found -> Not_found
      | Theory (mldb, dir, stanza) when coq_lang_version >= (0, 4) ->
        Found_stanza (mldb, dir, stanza)
      | Legacy_theory cp when coq_lang_version >= (0, 8) -> Found_path cp
      | Legacy_theory cp when Coq_path.stdlib cp -> Found_path cp
      | Legacy_theory _ -> Hidden LackOfInstalledComposition
      | Theory (mldb, dir, stanza) ->
        (match coq_db.resolve name with
         | Not_found -> Hidden LackOfTheoryComposition
         | Theory _ | Redirect _ | Legacy_theory _ -> Found_stanza (mldb, dir, stanza))
    ;;

    (** Our final final resolve is used externally, and should return the
        library data found from the previous iterations. *)
    let resolve coq_db ~coq_lang_version (loc, name) =
      match find coq_db ~coq_lang_version name with
      | Not_found -> Error.theory_not_found ~loc name
      | Hidden reason -> Error.hidden_without_composition ~loc ~reason name
      | Found_stanza (db, dir, stanza) ->
        let open Memo.O in
        let+ theory = create_from_stanza coq_db db dir stanza in
        let open Resolve.O in
        let* (_ : (Loc.t * Lib.t) list) = theory.libraries in
        let+ (_ : (Loc.t * lib) list) = theory.theories in
        Dune theory
      | Found_path cp ->
        let open Resolve.Memo.O in
        let boot_id = coq_db.boot_id in
        let+ theory = create_from_coqpath ~boot_id cp in
        Legacy theory
    ;;

    let resolve_boot coq_db ~coq_lang_version =
      let boot_id = boot_library_id coq_db in
      resolve_boot ~coq_lang_version ~coq_db boot_id
    ;;
  end

  include R

  let select_boot_id entries =
    match
      List.find_all entries ~f:(fun ((theory : Coq_stanza.Theory.t), _entry) ->
        theory.boot)
    with
    | [] -> None
    | [ ((theory, entry) : Coq_stanza.Theory.t * Entry.t) ] ->
      (match entry with
       | Theory path -> Some (Id.create ~path:(Path.build path) ~name:theory.name)
       | Redirect lib -> lib.boot_id)
    | boots ->
      let stanzas = List.map boots ~f:fst in
      Error.duplicate_boot_lib stanzas
  ;;

  let create_from_coqlib_stanzas
    ~(parent : t option)
    ~find_db
    (entries : (Coq_stanza.Theory.t * Entry.t) list)
    =
    let boot_id = select_boot_id entries in
    let map =
      match
        Coq_lib_name.Map.of_list_map
          entries
          ~f:(fun ((theory : Coq_stanza.Theory.t), entry) ->
            snd theory.name, (theory, entry))
      with
      | Ok m -> m
      | Error (_name, (theory1, entry1), (theory2, entry2)) ->
        let path entry =
          match (entry : Entry.t) with
          | Theory dir -> Path.build dir
          | Redirect db ->
            Code_error.raise "created stanza redirected to a library" [ "db", to_dyn db ]
        in
        let id1 = Id.create ~path:(path entry1) ~name:theory1.name in
        let id2 = Id.create ~path:(path entry2) ~name:theory2.name in
        Error.duplicate_theory_name id1 id2
    in
    let resolve name : t Resolve_result.t =
      match Coq_lib_name.Map.find map name with
      | None -> Not_found
      | Some (theory, entry) ->
        (match entry with
         | Theory dir -> Theory (find_db dir, dir, theory)
         | Redirect db -> Redirect db)
    in
    { boot_id; resolve; parent }
  ;;

  (* XXX: This is basically duplicated from create_from_coqlib_stanzas
     except for the slightly different input type *)
  let select_boot_path (cp : Coq_path.t list) =
    match List.find_all cp ~f:Coq_path.stdlib with
    | [] -> None
    | [ stdlib ] ->
      let name = Loc.none, Coq_path.name stdlib in
      let path = Coq_path.path stdlib in
      Some (Id.create ~path ~name)
    | paths -> Error.duplicate_boot_path paths
  ;;

  let create_from_coqpaths cps =
    (* No parent for coqpaths for now *)
    let parent = None in
    let boot_id = select_boot_path cps in
    (* Map is eager, this may not be very convenient in large trees,
       but should be fine for now *)
    let map =
      List.rev_map cps ~f:(fun cp -> Coq_path.name cp, cp)
      (* Since we reversed the order, the second coqpath should take
         precedence. This is in line with Coq semantics. *)
      |> Coq_lib_name.Map.of_list_reducei ~f:(fun _name _cp1 cp2 -> cp2)
    in
    let resolve name : t Resolve_result.t =
      match Coq_lib_name.Map.find map name with
      | None -> Not_found
      | Some cp -> Legacy_theory cp
    in
    { parent; resolve; boot_id }
  ;;

  (* Resolve helpers *)
  let find_many t theories ~coq_lang_version =
    Resolve.Memo.List.map theories ~f:(resolve ~coq_lang_version t)
  ;;
end

let theories_closure = function
  | Dune t -> Lazy.force t.theories_closure
  | Legacy _ -> Resolve.return []
;;
