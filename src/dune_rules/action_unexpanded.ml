open! Dune_engine
open! Stdune
open Import
module Mapper = Action_mapper.Make (Action_dune_lang) (Action_dune_lang)

let as_in_build_dir ~what ~loc p =
  match Path.as_in_build_dir p with
  | Some p -> p
  | None ->
    User_error.raise ~loc
      [ Pp.textf "%s %s is outside the build directory. This is not allowed."
          what
          (Path.to_string_maybe_quoted p)
      ]

module Action_expander : sig
  (* An applicative to help write action expansion. It is similar to
     [Action_builder.With_targets.t] but with some differences. The differences
     are as follow:

     - it allows to "consume" targets, which is required for interpreting
     [diff?]

     - files that are detected as both targets and dependencies are removed from
     the dependency set

     In addition to this, it embeds an expander for easily expanding templates. *)

  include Applicative_intf.S1

  (* Disable targets/dependencies inference detection *)
  val no_infer : 'a t -> 'a t

  val chdir : Path.Build.t -> 'a t -> 'a t

  val set_env : var:string -> value:string t -> (value:string -> 'a) t -> 'a t

  val run : 'a t -> expander:Expander.t -> 'a Action_builder.With_targets.t

  (* String with vars expansion *)
  module E : sig
    val string : String_with_vars.t -> string t

    val strings : String_with_vars.t -> string list t

    (* Evaluate a path in a position of target, such as in [(with-stdout-to
       <target> ...)] *)
    val target : String_with_vars.t -> Path.Build.t t

    (* Evaluate a path in a position of dependency, such as in [(cat <dep>)] *)
    val dep : String_with_vars.t -> Path.t t

    (* Evaluate a path in a position of optional dependency, such as in [(diff
       <dep_if_exists> ...)] *)
    val dep_if_exists : String_with_vars.t -> Path.t t

    (* Evaluate a path that is neither in a position of target or dependency,
       such as in [(chdir <path> ...)] *)
    val path : String_with_vars.t -> Path.t t

    (* Evaluate a path that "consumes" a target, such as in [(diff? ... <file>)] *)
    val consume_file : String_with_vars.t -> Path.Build.t t

    val prog_and_args : String_with_vars.t -> (Action.Prog.t * string list) t

    (* Expand a template statically and pass it to [f]. Raise if it cannot be
       expanded statically. *)
    val static_path : String_with_vars.t -> f:(Path.t -> 'a t) -> 'a t

    val static_string : String_with_vars.t -> f:(string -> 'a t) -> 'a t
  end
end = struct
  open Action_builder.O

  type deps =
    { static : Path.Set.t
    ; dyn : Path.Set.t Action_builder.t
    }

  type collector =
    { targets : Path.Build.Set.t
    ; deps : deps
    ; deps_if_exist : deps
    }

  type env =
    { expander : Expander.t
    ; infer : bool
    ; dir : Path.Build.t
    }

  type 'a t = env -> collector -> 'a Action_builder.t * collector

  let return x _env acc = (Action_builder.return x, acc)

  let map t ~f env acc =
    let b, acc = t env acc in
    (Action_builder.map b ~f, acc)

  let both a b env acc =
    let a, acc = a env acc in
    let b, acc = b env acc in
    (Action_builder.both a b, acc)

  let all =
    let rec loop res l env acc =
      match l with
      | [] -> (Action_builder.map (Action_builder.all res) ~f:List.rev, acc)
      | t :: l ->
        let x, acc = t env acc in
        loop (x :: res) l env acc
    in
    fun l env acc -> loop [] l env acc

  let run t ~expander =
    let deps =
      { static = Path.Set.empty; dyn = Action_builder.return Path.Set.empty }
    in
    let acc = { targets = Path.Build.Set.empty; deps; deps_if_exist = deps } in
    let env = { expander; infer = true; dir = Expander.dir expander } in
    let b, acc = t env acc in
    let { targets; deps; deps_if_exist } = acc in

    (* A file can be inferred as both a dependency and a target, for instance:

       {[ (progn (copy a b) (copy b c)) ]} *)
    let remove_targets =
      let targets =
        Path.Build.Set.to_list targets |> Path.Set.of_list_map ~f:Path.build
      in
      fun deps -> Path.Set.diff deps targets
    in
    let action_builder_path_set_if_exist deps_if_exist =
      Path.Set.fold deps_if_exist ~init:(Action_builder.return ())
        ~f:(fun f acc ->
          acc
          >>> Action_builder.if_file_exists f ~then_:(Action_builder.path f)
                ~else_:(Action_builder.return ()))
    in
    let add_deps f { static; dyn } =
      f (remove_targets static)
      >>> Action_builder.Expert.action_builder
            (let+ set = dyn in
             f (remove_targets set))
    in
    Action_builder.with_targets_set ~targets
      (let+ () = add_deps Action_builder.path_set deps
       and+ () = add_deps action_builder_path_set_if_exist deps_if_exist
       and+ res = b in
       res)

  let chdir dir t env acc =
    (* We do not change the directory of the expander to make sure payloads are
       interpreted relative to the directory of the dune file where the action
       is written.

       This is because in:

       {v $ cat a/dune

       (rule (alias default) (action (chdir b (run foo %{dep:p})))) v}

       [p] is treated as relative to "a" rather than "a/b".

       Note However that the expansion of %{dep:p} is done relative to the
       current working directory. i.e. [(run foo %{dep:p})] becomes [(run foo
       ../p)] because of the "chdir ..". *)
    t { env with dir } acc

  let set_env ~var ~value t env acc =
    let value, acc = value env acc in
    let value = Action_builder.memoize "env var" value in
    let env =
      { env with
        expander = Expander.set_local_env_var env.expander ~var ~value
      }
    in
    let f, acc = t env acc in
    let b =
      let+ f = f
      and+ value = value in
      f ~value
    in
    (b, acc)

  let no_infer t env acc =
    let x, _acc = t { env with infer = false } acc in
    (x, acc)

  module O = struct
    let ( let+ ) x f = map x ~f

    let ( and+ ) = both

    let ( >>> ) a b env acc =
      let a, acc = a env acc in
      let b, acc = b env acc in
      (Action_builder.O.( >>> ) a b, acc)
  end

  module E = struct
    let loc = String_with_vars.loc

    (* Override [Expander] to make sure to convert paths to strings relative to
       the current working directory rather than the directory of the expander.
       cf comment for [chdir]. *)
    module Expander = struct
      let expand env ~mode template =
        Action_builder.Expander.expand ~dir:(Path.build env.dir) ~mode template
          ~f:(Expander.expand_pform env.expander)

      let expand_path env sw =
        let+ v = expand env ~mode:Single sw in
        Value.to_path v ~error_loc:(String_with_vars.loc sw)
          ~dir:(Path.build env.dir)

      let expand_string env sw =
        let+ v = expand env ~mode:Single sw in
        Value.to_string v ~dir:(Path.build env.dir)

      let expand_strings env sw =
        let+ v = expand env ~mode:Many sw in
        Value.L.to_strings v ~dir:(Path.build env.dir)

      module Static = struct
        let expand env ~mode template =
          String_with_vars.expand ~dir:(Path.build env.dir) ~mode template
            ~f:(Expander.Static.expand_pform env.expander)

        let expand_path env sw =
          let v = expand env ~mode:Single sw in
          Value.to_path v ~error_loc:(String_with_vars.loc sw)
            ~dir:(Path.build env.dir)

        let expand_string env sw =
          let v = expand env ~mode:Single sw in
          Value.to_string v ~dir:(Path.build env.dir)

        module Or_exn = struct
          let expand_path env sw =
            Result.try_with (fun () -> expand_path env sw)

          let expand_string env sw =
            Result.try_with (fun () -> expand_string env sw)
        end
      end

      let artifacts = Expander.artifacts

      let map_exe = Expander.map_exe
    end

    let string sw env acc = (Expander.expand_string env sw, acc)

    let strings sw env acc = (Expander.expand_strings env sw, acc)

    let fail exn acc =
      (Action_builder.fail { fail = (fun () -> raise exn) }, acc)

    let static_string sw ~f env acc =
      match Expander.Static.Or_exn.expand_string env sw with
      | Ok s -> f s env acc
      | Error exn -> fail exn acc

    let path sw env acc = (Expander.expand_path env sw, acc)

    let static_path sw ~f env acc =
      match Expander.Static.Or_exn.expand_path env sw with
      | Ok s -> f s env acc
      | Error exn -> fail exn acc

    let register_dep x ~f env acc =
      if not env.infer then
        (x, acc)
      else
        (* Try to collect the dependency statically as it helps for [dune
           external-lib-deps]. *)
        match Action_builder.static_eval x with
        | Some (x, builder) -> (
          ( builder >>> Action_builder.return x
          , match f x with
            | None -> acc
            | Some fn ->
              { acc with
                deps =
                  { acc.deps with static = Path.Set.add acc.deps.static fn }
              } ) )
        | None
        | (exception User_error.E _) ->
          let x = Action_builder.memoize "dep" x in
          ( x
          , { acc with
              deps =
                { acc.deps with
                  dyn =
                    (let+ x = x
                     and+ set = acc.deps.dyn in
                     match f x with
                     | None -> set
                     | Some fn -> Path.Set.add set fn)
                }
            } )

    let dep sw env acc =
      let fn = Expander.expand_path env sw in
      register_dep fn ~f:Option.some env acc

    let dep_if_exists sw env acc =
      let fn = Expander.expand_path env sw in
      if not env.infer then
        (fn, acc)
      else
        match Action_builder.static_eval fn with
        | Some (fn, fn_builder) ->
          ( fn_builder >>> Action_builder.return fn
          , { acc with
              deps_if_exist =
                { acc.deps_if_exist with
                  static = Path.Set.add acc.deps_if_exist.static fn
                }
            } )
        | None
        | (exception User_error.E _) ->
          let fn = Action_builder.memoize "dep_if_exists" fn in
          ( fn
          , { acc with
              deps_if_exist =
                { acc.deps_if_exist with
                  dyn =
                    (let+ fn = fn
                     and+ set = acc.deps_if_exist.dyn in
                     Path.Set.add set fn)
                }
            } )

    let consume_file sw env acc =
      if not env.infer then
        let b, acc = path sw env acc in
        let b =
          let+ p = b in
          as_in_build_dir p ~what:"File" ~loc:(loc sw)
        in
        (b, acc)
      else
        static_path sw env acc ~f:(fun p _env acc ->
            let p = as_in_build_dir p ~what:"File" ~loc:(loc sw) in
            ( Action_builder.return p
            , { acc with targets = Path.Build.Set.remove acc.targets p } ))

    let target sw env acc =
      if not env.infer then
        ( (let+ p = Expander.expand_path env sw in
           as_in_build_dir ~what:"Target" ~loc:(loc sw) p)
        , acc )
      else
        match Expander.Static.Or_exn.expand_path env sw with
        | Error exn -> fail exn acc
        | Ok p ->
          let p = as_in_build_dir ~what:"Target" ~loc:(loc sw) p in
          ( Action_builder.return p
          , { acc with targets = Path.Build.Set.add acc.targets p } )

    let prog_and_args sw env acc =
      let b =
        let dir = Path.build env.dir in
        let loc = loc sw in
        let+ prog, args = Expander.expand env sw ~mode:At_least_one in
        let prog =
          match prog with
          | Value.Dir p ->
            User_error.raise ~loc
              [ Pp.textf "%s is a directory and cannot be used as an executable"
                  (Path.to_string_maybe_quoted p)
              ]
          | Path p -> Ok p
          | String s -> (
            match Filename.analyze_program_name s with
            | Relative_to_current_dir
            | Absolute ->
              Ok (Path.relative dir s)
            | In_path ->
              Artifacts.Bin.binary ~loc:(Some loc)
                (Expander.artifacts env.expander)
                s )
        in
        let prog = Result.map prog ~f:(Expander.map_exe env.expander) in
        let args = Value.L.to_strings ~dir args in
        (prog, args)
      in
      register_dep b env acc ~f:(function
        | Ok p, _ -> Some p
        | Error _, _ -> None)
  end
end

let rec expand (t : Action_dune_lang.t) : Action.t Action_expander.t =
  let module A = Action_expander in
  let module E = Action_expander.E in
  let open Action_expander.O in
  let module O (* [O] for "outcome" *) = Action in
  let expand_run prog args =
    let+ args = A.all (List.map args ~f:E.strings)
    and+ prog, more_args = E.prog_and_args prog in
    let args = List.concat args in
    (prog, more_args @ args)
  in
  match t with
  | Run (prog, args) ->
    let+ prog, args = expand_run prog args in
    O.Run (prog, args)
  | With_accepted_exit_codes (pred, t) ->
    let+ t = expand t in
    O.With_accepted_exit_codes (pred, t)
  | Dynamic_run (prog, args) ->
    let+ prog, args = expand_run prog args in
    O.Dynamic_run (prog, args)
  | Chdir (fn, t) ->
    E.static_path fn ~f:(fun dir ->
        A.chdir
          (as_in_build_dir dir ~loc:(String_with_vars.loc fn) ~what:"Directory")
          (let+ t = expand t in
           O.Chdir (dir, t)))
  | Setenv (var, value, t) ->
    E.static_string var ~f:(fun var ->
        A.set_env ~var ~value:(E.string value)
          (let+ t = expand t in
           fun ~value -> O.Setenv (var, value, t)))
  | Redirect_out (outputs, fn, t) ->
    let+ fn = E.target fn
    and+ t = expand t in
    O.Redirect_out (outputs, fn, t)
  | Redirect_in (inputs, fn, t) ->
    let+ fn = E.dep fn
    and+ t = expand t in
    O.Redirect_in (inputs, fn, t)
  | Ignore (outputs, t) ->
    let+ t = expand t in
    O.Ignore (outputs, t)
  | Progn l ->
    let+ l = A.all (List.map l ~f:expand) in
    O.Progn l
  | Echo xs ->
    let+ l = A.all (List.map xs ~f:E.strings) in
    let l = List.concat l in
    O.Echo l
  | Cat fn ->
    let+ fn = E.dep fn in
    O.Cat fn
  | Copy (x, y) ->
    let+ x = E.dep x
    and+ y = E.target y in
    O.Copy (x, y)
  | Symlink (x, y) ->
    let+ x = E.dep x
    and+ y = E.target y in
    O.Symlink (x, y)
  | Copy_and_add_line_directive (x, y) ->
    let+ x = E.dep x
    and+ y = E.target y in
    O.Copy_and_add_line_directive (x, y)
  | System x ->
    let+ x = E.string x in
    O.System x
  | Bash x ->
    let+ x = E.string x in
    O.Bash x
  | Write_file (fn, s) ->
    let+ fn = E.target fn
    and+ s = E.string s in
    O.Write_file (fn, s)
  | Rename (x, _) ->
    (* [Rename] is not part of the syntax so this case is not reachable. The
       reason it is not exposed to the user is because we can't easily decide
       what [x] is. i.e. it might be a file generated by the action and renamed
       immediately, or it could be a dependency. *)
    Code_error.raise ~loc:(String_with_vars.loc x)
      "Rename is not allowed in unexpanded actions" []
  | Remove_tree x ->
    (* Same kind of stuff for [Remove_tree] *)
    Code_error.raise ~loc:(String_with_vars.loc x)
      "Remove_tree is not allowed in unexpanded actions" []
  | Mkdir x ->
    (* This code path should in theory be unreachable too, but we don't delete
       it to remember about the check in in case we expose [mkdir] in the syntax
       one day. *)
    let+ path = E.path x in
    if not (Path.is_managed path) then
      User_error.raise ~loc:(String_with_vars.loc x)
        [ Pp.text
            "(mkdir ...) is not supported for paths outside of the workspace:"
        ; Pp.seq (Pp.verbatim "  ")
            (Dune_lang.pp
               (List
                  [ Dune_lang.unsafe_atom_of_string "mkdir"; Dpath.encode path ]))
        ];
    O.Mkdir path
  | Digest_files l ->
    let+ l = A.all (List.map l ~f:E.dep) in
    O.Digest_files l
  | Diff { optional; file1; file2; mode } ->
    let+ file1 = E.dep_if_exists file1
    and+ file2 =
      if optional then
        E.consume_file file2
      else
        let+ p = E.dep file2 in
        as_in_build_dir p ~loc:(String_with_vars.loc file2) ~what:"File"
    in
    O.Diff { optional; file1; file2; mode }
  | Merge_files_into (sources, extras, target) ->
    let+ sources = A.all (List.map sources ~f:E.dep)
    and+ extras = A.all (List.map extras ~f:E.string)
    and+ target = E.target target in
    O.Merge_files_into (sources, extras, target)
  | No_infer t -> A.no_infer (expand t)
  | Pipe (outputs, l) ->
    let+ l = A.all (List.map l ~f:expand) in
    O.Pipe (outputs, l)
  | Format_dune_file (ver, src, dst) ->
    let+ src = E.dep src
    and+ dst = E.target dst in
    O.Format_dune_file (ver, src, dst)
  | Cram script ->
    let+ script = E.dep script in
    O.Cram script

let expand t ~loc ~deps:deps_written_by_user ~targets_dir
    ~targets:targets_written_by_user ~expander =
  let open Action_builder.O in
  let deps_builder, expander =
    Dep_conf_eval.named ~expander deps_written_by_user
  in
  let expander =
    match (targets_written_by_user : Targets.Or_forbidden.t) with
    | Targets Infer
    | Forbidden _ ->
      (* TODO jeremiedimino: the error message used to be better when someone
         was using %{target} in an [(alias ...)] stanza. *)
      expander
    | Targets (Static { targets; multiplicity }) ->
      Expander.add_bindings_full expander
        ~bindings:
          (Pform.Map.singleton
             (Var
                ( match multiplicity with
                | One -> Target
                | Multiple -> Targets ))
             (Action_builder.return
                (Value.L.paths (List.map targets ~f:Path.build))))
  in
  let expander =
    Expander.set_expanding_what expander (User_action targets_written_by_user)
  in
  let { Action_builder.With_targets.build; targets } =
    Action_expander.run (expand t) ~expander
  in
  let pp_path_build target =
    Pp.text (Dpath.describe_path (Path.build target))
  in
  let targets =
    match (targets_written_by_user : Targets.Or_forbidden.t) with
    | Targets Infer -> targets
    | Targets (Static { targets = targets'; multiplicity = _ }) ->
      Path.Build.Set.union targets (Path.Build.Set.of_list targets')
    | Forbidden context ->
      if Path.Build.Set.is_empty targets then
        targets
      else
        User_error.raise ~loc
          [ Pp.textf
              "%s must not have targets, however I inferred that these files \
               will be created by this action:"
              (String.capitalize context)
          ; Pp.enumerate (Path.Build.Set.to_list targets) ~f:pp_path_build
          ]
  in
  Path.Build.Set.iter targets ~f:(fun target ->
      if Path.Build.( <> ) (Path.Build.parent_exn target) targets_dir then
        User_error.raise ~loc
          [ Pp.text
              "This action has targets in a different directory than the \
               current one, this is not allowed by dune at the moment:"
          ; Pp.enumerate (Path.Build.Set.to_list targets) ~f:pp_path_build
          ]);
  let build =
    let+ () = deps_builder
    and+ action = build in
    let dir = Path.build (Expander.dir expander) in
    Action.Chdir (dir, action)
  in
  Action_builder.with_targets_set ~targets build

(* We re-export [Action_dune_lang] in the end to avoid polluting the inferred
   types in this module with all the various t's *)
include Action_dune_lang
