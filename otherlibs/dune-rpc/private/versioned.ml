open Import
open Types

module Version_error = struct
  type t =
    { payload : Csexp.t option
    ; message : string
    }

  let payload t = t.payload

  let message t = t.message

  let create ?payload ~message () = { payload; message }

  exception E of t

  let to_response_error { payload; message } =
    Response.Error.create ~kind:Invalid_request ?payload ~message ()
end

module Staged = struct
  type ('req, 'resp) request =
    { encode_req : 'req -> Call.t
    ; decode_resp : Csexp.t -> ('resp, Response.Error.t) result
    }

  type 'payload notification = { encode : 'payload -> Call.t }
end

let raise_version_bug ~method_ ~selected ~verb ~known =
  Code_error.raise "bug with version negotiation; selected bad method version"
    [ ("message", Dyn.String ("version is " ^ verb))
    ; ("method", Dyn.String method_)
    ; ("implemented versions", Dyn.List (List.map ~f:(fun i -> Dyn.Int i) known))
    ; ("selected version", Dyn.Int selected)
    ]

(* Pack a universal map key. See below. We can afford to erase the type of
   the key, because we only care about the keyset of the stored generation
   listing. *)
type packed = T : 'a Method.Version.Map.t Univ_map.Key.t -> packed

module Make (Fiber : Fiber_intf.S) = struct
  module Handler = struct
    type 'state t =
      { menu : Menu.t
      ; handle_request :
          Menu.t -> 'state -> Types.Request.t -> Response.t Fiber.t
      ; handle_notification :
          Menu.t -> 'state -> Call.t -> (unit, Response.Error.t) result Fiber.t
      ; prepare_request :
          'req 'resp.
             Menu.t
          -> ('req, 'resp) Decl.Request.witness
          -> (('req, 'resp) Staged.request, Version_error.t) result
      ; prepare_notification :
          'a.
             Menu.t
          -> 'a Decl.Notification.witness
          -> ('a Staged.notification, Version_error.t) result
      }

    let handle_request t = t.handle_request t.menu

    let handle_notification t = t.handle_notification t.menu

    let prepare_request t = t.prepare_request t.menu

    let prepare_notification t = t.prepare_notification t.menu
  end

  (* TODO: This module involves some convoluted and difficult-to-understand
     types, with multiple levels of GADTs and type packing, in the
     (possibly-misguided) twin aims of ensuring type safety and maximizing reuse
     of the actual generation management code. *)
  module Builder = struct
    open Decl

    (* A [('req, 'resp) Decl.Generation.t] contains the information necessary to
       convert from a [Csexp.t] to a ['req]. The [_handler] packings are to
       enable storing the callbacks in a homogeneous data structure (namely, the
       [Method.Name.Table.t]. It's alright to erase these types, because these
       callbacks are intended to be used by the receiving endpoint, which only
       sees a [Csexp.t], and we only discover the correct type to deserialize to
       at runtime. *)
    type 's r_handler =
      | R :
          ('s -> 'req -> 'resp Fiber.t) * ('req, 'resp) Decl.Generation.t
          -> 's r_handler

    type 's n_handler =
      | N :
          ('s -> 'payload -> unit Fiber.t) * ('payload, unit) Decl.Generation.t
          -> 's n_handler

    (* The declarations and implementations serve dual purposes with dual
       requirements.

       When storing implementations, we erase the type of the callback, because
       we cannot know what type to deserialize to until runtime, and so all that
       matters is whether some handler with the correct type exists.

       On the other hand, declarations must keep some type information in an
       externally-retrievable way. This is because when invoking an RPC of type
       [('req, 'resp)], we are *given* a value of type ['req], so the object
       being stored in the map cannot have its type erased. Instead, we use a
       [Univ_map] (with the key being stored in the [Decl.t]) so we can retrieve
       a correctly-typed [Generation.t] mapping later.

       However, unlike a string table, the use of a [Univ_map.t] means that we
       cannot examine the map alone to get a list of all declared procedures and
       versions. This is bad, because we need that information to perform
       version negotiation for the session. To resolve this, we also keep a
       mapping of all known keys and their associated method names, which we use
       to construct the initial version menu, then discard. *)
    type 'state t =
      { mutable declared_requests : packed list Method.Name.Map.t * Univ_map.t
      ; mutable declared_notifications :
          packed list Method.Name.Map.t * Univ_map.t
      ; implemented_requests :
          'state r_handler Method.Version.Map.t Method.Name.Table.t
      ; implemented_notifications :
          'state n_handler Method.Version.Map.t Method.Name.Table.t
      }

    (* A [('state, 'key, 'output) field_witness] is a first-class representation
       of a field of a ['state t]. Each field is morally a mutable table holding
       ['output Method.Version.Map.t]s (mapping generation numbers to
       ['output]s), indexed by ['key]s.

       The mental model isn't strictly correct (mostly due to needing the "all
       known registered keys" hack described above), but is accurate enough that
       the types of [get] and [set] below should become readable.

       By doing things this way, we can abstract away the logic of

       - Checking the corresponding registry (the declarations table when
       implementing, and vice versa) for duplicate entries

       - Checking the provided generation listings for overlap

       and

       - Looking up a method name and generation number

       from the type-erasure implementation shenanigans described above, letting
       all related operations (declaring, implementing, dispatching) share
       uniform implementations as much as possible. *)
    type (_, _, _) field_witness =
      | Declared_requests
          : ( _
            , Method.Name.t
              * ('req, 'resp) Decl.Generation.t Method.Version.Map.t
                Univ_map.Key.t
            , ('req, 'resp) Decl.Generation.t )
            field_witness
      | Declared_notifs
          : ( _
            , Method.Name.t
              * ('a, unit) Decl.Generation.t Method.Version.Map.t Univ_map.Key.t
            , ('a, unit) Decl.Generation.t )
            field_witness
      | Impl_requests : ('state, string, 'state r_handler) field_witness
      | Impl_notifs : ('state, string, 'state n_handler) field_witness

    let get (type st a b) (t : st t) (witness : (st, a, b) field_witness)
        (key : a) : b Method.Version.Map.t option =
      match witness with
      | Declared_requests ->
        let _, key = key in
        let _, table = t.declared_requests in
        Univ_map.find table key
      | Declared_notifs ->
        let _, key = key in
        let _, table = t.declared_notifications in
        Univ_map.find table key
      | Impl_requests -> Method.Name.Table.find t.implemented_requests key
      | Impl_notifs -> Method.Name.Table.find t.implemented_notifications key

    let set (type st a b) (t : st t) (witness : (st, a, b) field_witness)
        (key : a) (value : b Method.Version.Map.t) =
      match witness with
      | Declared_requests ->
        let name, key = key in
        let known_keys, table = t.declared_requests in
        t.declared_requests <-
          ( Method.Name.Map.add_multi known_keys name (T key)
          , Univ_map.set table key value )
      | Declared_notifs ->
        let name, key = key in
        let known_keys, table = t.declared_notifications in
        t.declared_notifications <-
          ( Method.Name.Map.add_multi known_keys name (T key)
          , Univ_map.set table key value )
      | Impl_requests -> Method.Name.Table.set t.implemented_requests key value
      | Impl_notifs ->
        Method.Name.Table.set t.implemented_notifications key value

    let registered_procedures
        { declared_requests = declared_request_keys, declared_request_table
        ; declared_notifications =
            declared_notification_keys, declared_notification_table
        ; implemented_requests
        ; implemented_notifications
        } =
      let batch_declarations which declared_keys declaration_table =
        Method.Name.Map.foldi declared_keys ~init:[] ~f:(fun name keys acc ->
            let generations =
              List.fold_left keys ~init:[] ~f:(fun acc (T key) ->
                  match Univ_map.find declaration_table key with
                  | Some listing -> Method.Version.Map.keys listing @ acc
                  | None ->
                    Code_error.raise
                      "versioning: method found in versioning table without \
                       actually being declared"
                      [ ("method_", Dyn.String name)
                      ; ("table", Dyn.String ("known_" ^ which ^ "_table"))
                      ])
            in
            (name, generations) :: acc)
      in
      let declared_requests =
        batch_declarations "request" declared_request_keys
          declared_request_table
      in
      let declared_notifications =
        batch_declarations "notification" declared_notification_keys
          declared_notification_table
      in
      let batch_implementations table =
        Method.Name.Table.foldi table ~init:[] ~f:(fun name listing acc ->
            (name, Method.Version.Map.keys listing) :: acc)
      in
      let implemented_requests = batch_implementations implemented_requests in
      let implemented_notifications =
        batch_implementations implemented_notifications
      in
      List.concat
        [ declared_requests
        ; declared_notifications
        ; implemented_requests
        ; implemented_notifications
        ]

    let create () =
      let declared_requests = (Method.Name.Map.empty, Univ_map.empty) in
      let declared_notifications = (Method.Name.Map.empty, Univ_map.empty) in
      let implemented_requests = Method.Name.Table.create 16 in
      let implemented_notifications = Method.Name.Table.create 16 in
      { declared_requests
      ; declared_notifications
      ; implemented_requests
      ; implemented_notifications
      }

    let register_generic t ~method_ ~generations ~registry ~registry_key ~other
        ~other_key ~pack =
      let () =
        get t other other_key
        |> Option.iter ~f:(fun _ ->
               Code_error.raise "attempted to implement and declare method"
                 [ ("method", Dyn.String method_) ])
      in
      let prior_registered_generations =
        get t registry registry_key
        |> Option.value ~default:Method.Version.Map.empty
      in
      let all_generations, duplicate_generations =
        List.fold_left generations
          ~init:(prior_registered_generations, Method.Version.Set.empty)
          ~f:(fun (acc, dups) (n, gen) ->
            match Method.Version.Map.add acc n (pack gen) with
            | Error _ -> (acc, Method.Version.Set.add dups n)
            | Ok acc' -> (acc', dups))
      in
      if Method.Version.Set.is_empty duplicate_generations then
        set t registry registry_key all_generations
      else
        Code_error.raise
          "attempted to register duplicate generations for RPC method"
          [ ("method", Dyn.String method_)
          ; ("duplicated", Method.Version.Set.to_dyn duplicate_generations)
          ]

    let declare_request t proc =
      register_generic t ~method_:proc.Request.decl.method_
        ~generations:proc.Request.generations ~registry:Declared_requests
        ~other:Impl_requests
        ~registry_key:(proc.Request.decl.method_, proc.decl.key)
        ~other_key:proc.Request.decl.method_ ~pack:Fun.id

    let declare_notification t (proc : _ notification) =
      register_generic t ~method_:proc.decl.method_
        ~generations:proc.generations ~registry:Declared_notifs
        ~other:Impl_notifs
        ~registry_key:(proc.decl.method_, proc.decl.key)
        ~other_key:proc.decl.method_ ~pack:Fun.id

    let implement_request t (proc : _ request) f =
      register_generic t ~method_:proc.decl.method_
        ~generations:proc.generations ~registry:Impl_requests
        ~other:Declared_requests ~registry_key:proc.decl.method_
        ~other_key:(proc.decl.method_, proc.decl.key) ~pack:(fun r -> R (f, r))

    let implement_notification t (proc : _ notification) f =
      register_generic t ~method_:proc.decl.method_
        ~generations:proc.generations ~registry:Impl_notifs
        ~other:Declared_notifs ~registry_key:proc.decl.method_
        ~other_key:(proc.decl.method_, proc.decl.key) ~pack:(fun n -> N (f, n))

    let lookup_method_generic t ~menu ~table ~key ~method_ k s =
      match (get t table key, Menu.find menu method_) with
      | Some subtable, Some version -> s (subtable, version)
      | None, _ ->
        let payload = Sexp.record [ ("method", Atom method_) ] in
        k (Version_error.create ~message:"invalid method" ~payload ())
      | _, None ->
        let payload = Sexp.record [ ("method", Atom method_) ] in
        k
          (Version_error.create
             ~message:"remote and local have no common version for method"
             ~payload ())

    let to_handler t ~session_version =
      let open Fiber.O in
      let handle_request menu state (_id, (n : Call.t)) =
        lookup_method_generic t ~menu ~table:Impl_requests ~key:n.method_
          ~method_:n.method_
          (fun e -> Fiber.return (Error (Version_error.to_response_error e)))
          (fun (handlers, version) ->
            match Method.Version.Map.find handlers version with
            | None ->
              raise_version_bug ~method_:n.method_ ~selected:version
                ~verb:"unimplemented"
                ~known:(Method.Version.Map.keys handlers)
            | Some (R (f, T gen)) -> (
              match
                Conv.of_sexp gen.req ~version:(session_version state) n.params
              with
              | Error e -> Fiber.return (Error (Response.Error.of_conv e))
              | Ok req ->
                let+ resp = f state (gen.upgrade_req req) in
                Ok (Conv.to_sexp gen.resp (gen.downgrade_resp resp))))
      in
      let handle_notification menu state (n : Call.t) =
        lookup_method_generic t ~menu ~table:Impl_notifs ~key:n.method_
          ~method_:n.method_
          (fun e -> Fiber.return (Error (Version_error.to_response_error e)))
          (fun (handlers, version) ->
            match Method.Version.Map.find handlers version with
            | None ->
              raise_version_bug ~method_:n.method_ ~selected:version
                ~verb:"unimplemented"
                ~known:(Method.Version.Map.keys handlers)
            | Some (N (f, T gen)) -> (
              match
                Conv.of_sexp gen.req ~version:(session_version state) n.params
              with
              | Error e -> Fiber.return (Error (Response.Error.of_conv e))
              | Ok req ->
                let+ () = f state (gen.upgrade_req req) in
                Ok ()))
      in
      let prepare_request (type a b) menu (decl : (a, b) Decl.Request.witness) :
          ((a, b) Staged.request, Version_error.t) result =
        let method_ = decl.method_ in
        lookup_method_generic t ~menu ~table:Declared_requests
          ~key:(method_, decl.key) ~method_
          (fun e -> Error e)
          (fun (decls, version) ->
            match Method.Version.Map.find decls version with
            | None ->
              raise_version_bug ~method_ ~selected:version ~verb:"undeclared"
                ~known:(Method.Version.Map.keys decls)
            | Some (T gen) ->
              let encode_req (req : a) =
                { Call.method_
                ; params = Conv.to_sexp gen.req (gen.downgrade_req req)
                }
              in
              let decode_resp sexp =
                match Conv.of_sexp gen.resp ~version:(3, 0) sexp with
                | Ok resp -> Ok (gen.upgrade_resp resp)
                | Error e -> Error (Response.Error.of_conv e)
              in
              Ok { Staged.encode_req; decode_resp })
      in
      let prepare_notification (type a) menu
          (decl : a Decl.Notification.witness) :
          (a Staged.notification, Version_error.t) result =
        let method_ = decl.method_ in
        lookup_method_generic t ~menu ~table:Declared_notifs
          ~key:(method_, decl.key) ~method_
          (fun e -> Error e)
          (fun (decls, version) ->
            match Method.Version.Map.find decls version with
            | None ->
              raise_version_bug ~method_ ~selected:version ~verb:"undeclared"
                ~known:(Method.Version.Map.keys decls)
            | Some (T gen) ->
              let encode (req : a) =
                { Call.method_
                ; params = Conv.to_sexp gen.req (gen.downgrade_req req)
                }
              in
              Ok { Staged.encode })
      in
      fun ~menu ->
        { Handler.menu
        ; handle_request
        ; handle_notification
        ; prepare_request
        ; prepare_notification
        }
  end
end
