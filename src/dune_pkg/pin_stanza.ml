open Import

module Package = struct
  type t =
    { version : Package_version.t option
    ; name : Package_name.t
    ; loc : Loc.t
    }

  let decode =
    let open Dune_lang.Decoder in
    fields
    @@
    let+ name = field "name" Package_name.decode
    and+ loc = loc
    and+ version = field_o "version" Package_version.decode in
    { name; version; loc }
  ;;

  let to_local_package t ~url ~origin =
    { Local_package.url
    ; version = Option.value ~default:Package_version.dev t.version
    ; loc = t.loc
    ; origin
    ; name = t.name
    }
  ;;
end

type t =
  { url : Loc.t * OpamUrl.t
  ; packages : Package.t list
  }

let url t = t.url

let common_fields =
  let open Dune_lang.Decoder in
  let+ url = field "url" OpamUrl.decode_loc
  and+ packages = multi_field "package" Package.decode in
  { url; packages }
;;

let decode = Dune_lang.Decoder.fields common_fields

let decode_with_name =
  let open Dune_lang.Decoder in
  fields
  @@
  let+ t = common_fields
  and+ name = field "name" (located string) in
  name, t
;;

module DB = struct
  type context =
    | Workspace
    | Project of { dir : Path.Source.t }

  type nonrec t =
    { all : t list (* we keep this for [encode] *)
    ; map : (Local_package.pin * context) Package_name.Map.t
    ; context : context
    }

  let empty = { all = []; map = Package_name.Map.empty; context = Workspace }
  let to_dyn = Dyn.opaque
  let hash = Poly.hash
  let equal = Poly.equal

  let add_opam_pins t pins =
    let pins =
      Package_name.Map.map pins ~f:Local_package.of_package |> Pinned_package.collect
    in
    { t with
      map =
        Package_name.Map.fold pins ~init:t.map ~f:(fun (pin : Local_package.pin) acc ->
          (match pin.origin with
           | `Dune -> Code_error.raise "add_opam_pins: can only pin opam packages" []
           | `Opam -> ());
          Package_name.Map.update acc pin.name ~f:(function
            | None -> Some (pin, t.context)
            | Some _ as x -> x))
    }
  ;;

  let package_map_of_list list ~pin =
    match Package_name.Map.of_list list with
    | Ok map -> map
    | Error (name, p, _) ->
      let pin : Local_package.pin = pin p in
      User_error.raise
        ~loc:pin.loc
        [ Pp.textf "package %S is already defined" (Package_name.to_string name) ]
  ;;

  let gen_decode context =
    let open Dune_lang.Decoder in
    let+ all = multi_field "pin" decode in
    let map =
      List.concat_map all ~f:(fun source ->
        List.map source.packages ~f:(fun (package : Package.t) ->
          let name = package.name in
          let package = Package.to_local_package package ~url:source.url ~origin:`Dune in
          name, (package, context)))
      |> package_map_of_list ~pin:fst
    in
    { all; map; context }
  ;;

  let decode ~dir = gen_decode (Project { dir })

  let super_context ((_, ctx) as x) ((_, ctx') as x') =
    match ctx, ctx' with
    | Workspace, Workspace -> Code_error.raise "more than one workspace context" []
    | Workspace, _ -> Ok x
    | _, Workspace -> Ok x'
    | Project { dir }, Project { dir = dir' } ->
      if Path.Source.equal dir dir'
      then
        Code_error.raise
          "two projects in the same directory in the workspace"
          [ "dir", Path.Source.to_dyn dir ]
      else if Path.Source.is_descendant dir ~of_:dir'
      then Ok x'
      else if Path.Source.is_descendant dir' ~of_:dir
      then Ok x
      else Error ()
  ;;

  let combine_exn t { all; map; context } =
    { all = t.all @ all
    ; map =
        Package_name.Map.union t.map map ~f:(fun name lhs rhs ->
          match super_context lhs rhs with
          | Ok res -> Some res
          | Error () ->
            let pin = fst lhs in
            User_error.raise
              ~loc:pin.loc
              [ Pp.textf
                  "package %S is defined in more than one source"
                  (Package_name.to_string name)
              ; Pp.textf "it is also defined in %s" (Loc.to_file_colon_line (fst lhs).loc)
              ])
    ; context
    }
  ;;

  let encode _ = (* CR-rgrinberg: needed for dune init *) []

  module Workspace = struct
    type nonrec t = Local_package.pin Package_name.Map.t String.Map.t

    let empty = String.Map.empty

    let decode =
      let open Dune_lang.Decoder in
      let+ pins = Dune_lang.Decoder.multi_field "pin" decode_with_name in
      match
        String.Map.of_list_map pins ~f:(fun ((loc, name), pin) ->
          let packages =
            List.map pin.packages ~f:(fun (package : Package.t) ->
              let package = Package.to_local_package package ~url:pin.url ~origin:`Dune in
              package.name, package)
            |> package_map_of_list ~pin:Fun.id
          in
          name, (loc, packages))
      with
      | Ok s -> String.Map.map ~f:snd s
      | Error (name, ((loc, _), _), _) ->
        User_error.raise ~loc [ Pp.textf "a pin named %S already defined" name ]
    ;;

    let extract (t : t) ~names =
      let map =
        List.concat_map names ~f:(fun (loc, name) ->
          match String.Map.find t name with
          | None -> User_error.raise ~loc [ Pp.textf "pin %S doesn't exist" name ]
          | Some packages ->
            Package_name.Map.to_list_map packages ~f:(fun name package ->
              name, (package, Workspace)))
        |> package_map_of_list ~pin:fst
      in
      { all = []; map; context = Workspace }
    ;;

    let equal = String.Map.equal ~equal:Poly.equal
    let to_dyn = Dyn.opaque
    let hash = Poly.hash
  end
end

module Scan_project = struct
  type t =
    read:(Path.Source.t -> string Fiber.t)
    -> files:Filename.Set.t
    -> (DB.t * Dune_lang.Package.t Package_name.Map.t) option Fiber.t

  type state =
    { traversed :
        (OpamUrl.t, (DB.t * Dune_lang.Package.t Package_name.Map.t) option) Fiber_cache.t
    }

  let make_state () = { traversed = Fiber_cache.create (module OpamUrl) }

  let eval_url t state (loc, url) =
    Fiber_cache.find_or_add state.traversed url ~f:(fun () ->
      let open Fiber.O in
      let* mount = Mount.of_opam_url loc url in
      let* files =
        Mount.readdir mount Path.Local.root
        >>| Filename.Map.filter ~f:(function
          | `File -> true
          | `Dir -> false)
        >>| Filename.Set.of_keys
      in
      let read path =
        let path = Path.Source.to_local path in
        Mount.read mount path
        >>| function
        | Some s -> s
        | None ->
          Code_error.raise
            "expected file to exist"
            [ "path", Path.Local.to_dyn path; "url", OpamUrl.to_dyn url ]
      in
      t ~read ~files)
  ;;
end

open DB

module Stack : sig
  type t

  val empty : t
  val pp : t -> _ Pp.t
  val is_prefix : t -> prefix:t -> bool
  val push : t -> Local_package.pin -> t
end = struct
  module Id = Id.Make ()

  type element =
    { id : Id.t
    ; pin : Local_package.pin
    }

  type t = element list

  let empty = []

  let is_prefix (t : t) ~(prefix : t) =
    match prefix with
    | [] -> true
    | p :: _ -> List.mem t p ~equal:(fun x y -> Id.equal x.id y.id)
  ;;

  let pp (t : t) =
    Pp.chain t ~f:(fun { pin; _ } ->
      Pp.textf
        "URL %s for package %s in %s"
        (OpamUrl.to_string (snd pin.url))
        (Package_name.to_string pin.name)
        (Loc.to_file_colon_line pin.loc))
  ;;

  let push (t : t) (package : Local_package.pin) : t =
    { id = Id.gen (); pin = package } :: t
  ;;
end

let resolve (t : DB.t) ~(scan_project : Scan_project.t)
  : Resolved_package.t Package_name.Map.t Fiber.t
  =
  let open Fiber.O in
  let* () = Fiber.return () in
  (* Assigned packages cannot be traversed because we already assigned
     them to a source.
     Invariant: a workspace's pins are assigned before all of its children *)
  let assigned = ref Package_name.Map.empty in
  (* The concrete opam metadata we determined for every assigned package. *)
  let resolved = ref Package_name.Map.empty in
  let resolve name resolved_package =
    resolved := Package_name.Map.add_exn !resolved name resolved_package
  in
  let assign (stack : Stack.t) (package : Local_package.pin) =
    match Package_name.Map.find !assigned package.name with
    | None ->
      assigned := Package_name.Map.add_exn !assigned package.name (package, stack);
      `Continue
    | Some (assigned, prefix) ->
      if Stack.is_prefix stack ~prefix
         || (OpamUrl.equal (snd assigned.url) (snd package.url)
             && Package_version.equal package.version assigned.version)
      then `Skip
      else
        (* CR-rgrinberg: we need to cancel all the other fibers *)
        User_error.raise
          ~loc:package.loc
          [ Pp.textf
              "package %s is assigned more than once"
              (Package_name.to_string package.name)
          ; Pp.textf
              "it's already assigned to %s at:"
              (OpamUrl.to_string (snd package.url))
          ; Pp.verbatim (Loc.to_file_colon_line assigned.loc)
          ; Pp.text "prefix"
          ; Stack.pp prefix
          ; Pp.text "stack"
          ; Stack.pp stack
          ]
  in
  let opam_package stack (package : Local_package.pin) =
    let* resolved_package = Pinned_package.resolve_package package in
    resolve package.name resolved_package;
    Resolved_package.opam_file resolved_package
    |> OpamFile.OPAM.pin_depends
    |> List.filter_map ~f:(fun (pkg, url) ->
      let name = Package_name.of_opam_package_name (OpamPackage.name pkg) in
      let package =
        let version =
          OpamPackage.version pkg |> Package_version.of_opam_package_version
        in
        { Local_package.url = package.loc, url
        ; version
        ; name
        ; loc = package.loc
        ; origin = `Opam
        }
      in
      let stack = Stack.push stack package in
      match assign stack package with
      | `Skip -> None
      | `Continue -> Some package)
    |> Fiber.parallel_iter ~f:(fun package ->
      Pinned_package.resolve_package package >>| resolve package.name)
  in
  let dune_package packages (package : Local_package.pin) =
    match Package_name.Map.find packages package.name with
    | None ->
      User_error.raise
        ~loc:package.loc
        [ Pp.textf
            "package %s doesn't exist in source %s"
            (Package_name.to_string package.name)
            (OpamUrl.to_string (snd package.url))
        ]
    | Some pkg ->
      let resolved_package =
        let opam_file =
          Local_package.of_package pkg
          |> Local_package.for_solver
          |> Local_package.For_solver.to_opam_file
          |> OpamFile.OPAM.with_url (OpamFile.URL.create (snd package.url))
        in
        let opam_package =
          OpamPackage.create
            (Package_name.to_opam_package_name package.name)
            (Package_version.to_opam_package_version package.version)
        in
        Resolved_package.dune_package package.loc opam_file opam_package
      in
      resolve package.name resolved_package
  in
  let eval_url =
    let state = Scan_project.make_state () in
    Scan_project.eval_url scan_project state
  in
  let rec loop (stack : Stack.t) t =
    Package_name.Map.values t.map
    |> List.filter_map ~f:(fun (package, _) ->
      match assign stack package with
      | `Skip -> None
      | `Continue -> Some package)
    |> Fiber.parallel_iter ~f:(fun (package : Local_package.pin) ->
      let stack = Stack.push stack package in
      match package.origin with
      | `Opam -> opam_package stack package
      | `Dune ->
        eval_url package.url
        >>= (function
         | None -> opam_package stack package
         | Some (more_sources, packages) ->
           dune_package packages package;
           let more_sources = DB.add_opam_pins more_sources packages in
           loop stack more_sources))
  in
  let+ () = loop Stack.empty t in
  !resolved
;;
