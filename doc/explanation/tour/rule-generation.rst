Rule Generation
---------------

Using these parsed stanzas, the next step is to generate rules. This work
starts in :file:`src/dune_rules/gen_rules.ml`, which dispatches to various
modules in :file:`src/dune_rules/`.

Rules are registered on the build engine using the following function from the
``Super_context`` module:

.. code-block:: ocaml

  val add_rule
    :  t
    -> ?mode:Rule.Mode.t
    -> ?loc:Loc.t
    -> dir:Path.Build.t
    -> Action.Full.t Action_builder.With_targets.t
    -> unit Memo.t

A value of ``Super_context.t`` represents an OCaml toolchain (``Context.t``) as
well as various capabilities to expand variables and refer to :doc:`(env)
stanzas </reference/dune/env>`. The last, unlabelled argument corresponds to
the fully annotated action. We'll go through its type below.

The modules in :file:`src/dune_rules` often expose a function ``gen_rules``
taking a parsed stanza, a ``Super_context.t`` value, a directory name (and
other arguments), and returning ``unit Memo.t``.

.. note::

  The ``Memo`` module is central to how Dune operates. It is a monadic
  memoization framework that allows two things:

  - Sharing and caching expensive internal computations, such as computing the
    list of libraries Dune knows about, or computing the list of flags that
    should be used to compile a given module.
  - Incremental recomputation of this cached data. ``Memo`` tracks dependencies
    between memoized values and will only recompute the necessary ones when an
    input changes. This is a mini in-memory build system that works like a
    spreadsheet. It is essential to the watch mode.

An example rule is the :doc:`/reference/dune/mdx` stanza, implemented in
:file:`src/dune_rules/mdx.ml`. There are several steps in setting up rules and
alias actions for a ``(mdx)`` stanza:

- Run ``ocaml-mdx deps`` on the input file to discover extra dependencies.
- Run ``ocaml-mdx dune-gen`` to produce a ``mdx_gen.ml-gen`` OCaml source file.
- Compile this generated executable.
- Attach an action to :doc:`/reference/aliases/runtest` that creates the
  ``.corrected`` file and immediately compares it with the original file using
  :doc:`/reference/actions/diff`.

Let's walk through these pieces.

The rule that generates ``mdx_gen.ml-gen`` is a regular rule made by running a
command:

.. code-block:: ocaml

  Command.run_dyn_prog
    ~dir:(Path.build dir)
    mdx_prog
    ~stdout_to
    [ A "dune-gen"
    ; prelude_args
    ; Resolve.Memo.args directory_args
    ; Lazy.force color_always
    ]

Here ``mdx_prog`` is the resolved path to ``ocaml-mdx``. It can point to a
binary in ``PATH`` or to a binary built in the current workspace. The command
arguments are written in a domain-specific language defined in
:file:`src/dune_rules/command.mli`. For example, ``A`` is a plain string, and
``Path`` or ``Dep`` arguments let Dune track paths used by the command. The
``~stdout_to`` parameter tells Dune which target this rule produces.

The generated executable is then built with ``Exe.build_and_link``.

The dependencies reported by ``ocaml-mdx deps`` are not stored in a separate
``.mdx.deps`` target. Instead, Dune runs that command as an anonymous action and
reads its standard output:

.. code-block:: ocaml

  let read ~sctx ~dir ~loc ~mdx_prog (files : Files.t) =
    let open Action_builder.O in
    (let* prog = mdx_prog in
     Command.run'
       ~dir:(Path.build dir)
       prog
       [ Command.Args.A "deps"
       ; Lazy.force color_always
       ; Dep (Path.build files.src)
       ])
    |> Super_context.execute_action_stdout sctx ~loc ~dir
    |> Action_builder.of_memo
    >>| parse

``Super_context.execute_action_stdout`` executes the command as an anonymous
action and returns its captured standard output to the rule generator. The
result is parsed into the dynamic dependencies of the mdx test action.

Finally, Dune attaches one anonymous action to the ``@runtest`` alias. This
action runs mdx to produce the ``.corrected`` file and then runs the diff action
that compares it with the source file:

.. code-block:: ocaml

  let mdx_action =
    let mdx_input_dependencies = (* Deps.read ... *) in
    let executable, command_line, redirect_stdout = (* ... *) in
    let env, sandbox = (* ... *) in
    let open Action_builder.O in
    let action =
      Action_builder.env_var "MDX_RUN_NON_DETERMINISTIC"
      >>> (Action_builder.map mdx_input_dependencies ~f:(fun d -> (), d)
           |> Action_builder.dyn_deps)
      >>> let* executable = executable in
          Command.run'
            ~dir:(Path.build dir)
            ~env
            ~sandbox
            executable
            command_line
    in
    let action =
      if redirect_stdout
      then
        Action_builder.map
          action
          ~f:(Action.Full.map ~f:(Action.with_stdout_to files.corrected))
      else action
    in
    let+ action
    and+ locks = Expander.expand_locks expander stanza.locks in
    let mkdir_corrected_dir =
      Action.mkdir (Path.Build.parent_exn files.corrected) |> Action.Full.make
    in
    let diff =
      Action.diff ~optional:false (Path.build files.src) files.corrected
      |> Action.Full.make
    in
    Action.Full.reduce
      [ mkdir_corrected_dir; Action.Full.add_locks locks action; diff ]
  in
  Super_context.add_alias_action
    sctx
    (Alias.make Alias0.runtest ~dir)
    mdx_action
    ~loc
    ~dir

Here, ``Action_builder.dyn_deps`` makes the dependencies discovered by
``ocaml-mdx deps`` part of the anonymous action. The ``.corrected`` file is an
intermediate file used by the same alias action, not a target with its own
standalone build rule. If the diff finds a mismatch, it registers the usual
promotion from the corrected file back to the source file.

.. note::

  ``Action_builder`` is different from ``Memo``, which corresponds to what
  happens within Dune itself. But it is also possible to use ``Memo`` from an
  ``Action_builder`` context. In that sense, ``Action_builder`` is more
  powerful: at execution time, ``Action_builder`` will manage what happens in
  the ``_build`` directory, while ``Memo`` is only concerned with what happens
  in memory.
