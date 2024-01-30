Locks
=====

.. TODO(diataxis)
   - howto: testing in general (note about concurrency)
   - reference: locks

Given two rules that are independent, Dune will assume that their
associated actions can be run concurrently. Two rules are considered
independent if neither of them depend on the other, either directly or
through a chain of dependencies. This basic assumption allows Dune to
parallelize the build.

However, it is sometimes the case that two independent rules cannot be
executed concurrently. For instance, this can happen for more
complicated tests. In order to prevent Dune from running the
actions at the same time, you can specify that both actions take the
same lock:

.. code:: dune

    (rule
     (alias  runtest)
     (deps   foo)
     (locks  m)
     (action (run test.exe %{deps})))

    (alias
     (rule   runtest)
     (deps   bar)
     (locks  m)
     (action (run test.exe %{deps})))

Dune will make sure that the executions of ``test.exe foo`` and
``test.exe bar`` are serialized.

Although they don't live in the filesystem, lock names are interpreted as file
names. So for instance, ``(with-lock m ...)`` in ``src/dune`` and ``(with-lock
../src/m)`` in ``test/dune`` refer to the same lock.

Note also that locks are per build context. So if your workspace has two build
contexts setup, the same rule might still be executed concurrently between the
two build contexts. If you want a lock that is global to all build contexts,
simply use an absolute filename:

.. code:: dune

    (rule
     (alias   runtest)
     (deps   foo)
     (locks  /tcp-port/1042)
     (action (run test.exe %{deps})))

