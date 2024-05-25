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

An example of rule is the :doc:`/reference/dune/mdx` stanza, implemented in
:file:`src/dune_rules/mdx.ml`. There are several steps in setting up rules for
a ``(mdx)`` stanza:

- How to run ``ocaml-mdx deps`` on the input file to produce a ``.mdx.deps``
- Run ``ocaml-mdx dune-gen`` to produce a ``mdx_gen.ml-gen`` OCaml source file
- Compile this executable
- Run this executable to produce a ``.corrected`` file
- Register a :doc:`/reference/actions/diff` action between the ``.corrected`` file
  and the original file

Let's walk through these rules.

The first one is about producing a ``.mdx.deps`` file. It is a simple call to
``Super_context.add_rule``.

.. code-block:: ocaml
  :linenos:
  :lineno-start: 312

  let* () = Super_context.add_rule sctx ~loc ~dir (Deps.rule ~dir ~mdx_prog files)

``Deps.rule`` is defined in a helper function:

.. code-block:: ocaml
  :linenos:
  :lineno-start: 77

  let rule ~dir ~mdx_prog (files : Files.t) =
    Command.run_dyn_prog
      ~dir:(Path.build dir)
      mdx_prog
      ~stdout_to:files.deps
      [ Command.Args.A "deps"; Lazy.force color_always; Dep (Path.build files.Files.src) ]

This is a rule made by just running a command, here ``mdx_prog`` (a resolved
path to ``ocaml-mdx``, meaning it can point to a binary in ``PATH`` or a built
version in the current workspace). Its arguments are a domain-specific language
defined in :file:`src/dune_rules/command.mli` where ``A`` refers to a plain
string, and ``Dep`` refers to a string that should be interpreted as a dependency.
Between that, and the ``~stdout_to`` parameter, it is enough for Dune to know
about the rule's dependencies (what it will read) and its target (what it will
produce).

The second rule, which generates ``mdx_gen.ml-gen``, is similar. It is also done
by calling ``Command.run_dyn_prog``.

The third rule, to build the executable, calls ``Exe.build_and_link`` that is a
helper function.

Let's observe how the fourth rule (that calls the generated executable) is set
up.

.. code-block:: ocaml

    let mdx_action ~loc:_ =
      let open Action_builder.With_targets.O in
      let mdx_input_dependencies = (* ... *) in
      let executable, command_line = (* ... *) in
      let deps, sandbox = (* ... *) in
      let+ action =
        Action_builder.with_no_targets deps
        >>> Action_builder.with_no_targets
              (Action_builder.env_var "MDX_RUN_NON_DETERMINISTIC")
        >>> Action_builder.with_no_targets
              (Action_builder.map mdx_input_dependencies ~f:(fun d -> (), d)
               |> Action_builder.dyn_deps)
        >>> Command.run_dyn_prog
              ~dir:(Path.build dir)
              ~stdout_to:files.corrected
              executable
              command_line
      and+ locks =
        Expander.expand_locks expander stanza.locks |> Action_builder.with_no_targets
      in
      Action.Full.add_locks locks action |> Action.Full.add_sandbox sandbox
    in
    Super_context.add_rule sctx ~loc ~dir (mdx_action ~loc)

Here, the ``mdx_action`` that is set up is not just a single
``Command.run_dyn_prog`` call. It is assembled using combinators from
``Action_builder.With_targets``. This is another monad used in Dune. It
corresponds to what can happen at build time, like running commands or creating
files, or more complex actions such as reading a file that needs to be built by
another rule. It is also used to track dependencies and targets. The "thing"
that we register to the Dune engine using ``Super_context.add_rule`` has type
``Action.Full.t Action_builder.With_targets.t``.

.. note::

  This is different from ``Memo``, which corresponds to what happens within
  Dune itself. But it is also possible to use ``Memo`` from an
  ``Action_builder`` context. In that sense, ``Action_builder`` is more
  powerful: at execution time, ``Action_builder`` will manage what happens in
  the ``_build`` directory, while ``Memo`` is only concerned with what happens
  in memory.

Finally, to register the correction, the technique is to attach the
:doc:`/reference/actions/diff` action to the :doc:`/reference/aliases/runtest`
alias (a collection of rules) using this call:

.. code-block:: ocaml
  :linenos:
  :lineno-start: 405

  (* Attach the diff action to the @runtest for the src and corrected files *)
  Files.diff_action files
  |> Super_context.add_alias_action sctx (Alias.make Alias0.runtest ~dir) ~loc ~dir

Where ``Files.diff_action`` is defined as:

.. code-block:: ocaml
  :linenos:
  :lineno-start: 33

  let diff_action { src; corrected; deps = _ } =
    let src = Path.build src in
    let open Action_builder.O in
    let+ () = Action_builder.path src
    and+ () = Action_builder.path (Path.build corrected) in
    Action.Full.make (Action.diff ~optional:false src corrected)
  ;;

As explained above, ``Action_builder`` keeps tracks of dependencies, so using
``let+ () = Action_builder.path src`` is a way to declare ``src`` as a
dependency of the current action.
