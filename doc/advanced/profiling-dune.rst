Profiling Dune
==============

.. TODO(diataxis)
   - reference: the CLI
   - howto: profiling a dune build

Dune writes detailed trace data about internal operations (such as command
timing) to ``_build/trace.csexp`` by default. Use ``--trace-file FILE`` to
write to a different location.

Allocation Profiling
--------------------

On supported OCaml versions, enable sampled allocation profiles with
``DUNE_TRACE=+alloc``. Allocation summaries are included in the trace after
each build and when Dune exits.

The experimental ``DUNE_TRACE_ALLOC`` variable accepts a comma-separated list
of profiler settings:

.. code:: console

   $ DUNE_TRACE="+alloc" \
     DUNE_TRACE_ALLOC="rate=0.001,stack=20,top=100" \
     dune build

``rate`` is the number of samples per allocated word, ``stack`` is the maximum
recorded call-stack depth, and ``top`` is the maximum number of entries kept in
each ranking. Their defaults are ``0.0001``, ``10``, and ``10``. This
configuration is experimental and may change without notice.

Each sampled heap is ranked by exact call stack, immediate allocation site, and
inclusive stack frame. The allocation-site view combines allocations at the
same source location that have different callers. The inclusive-frame view
identifies subsystems responsible for allocations below them in the call
stack. Summing the samples in a truncated exact-stack or allocation-site
ranking and dividing by ``total_samples`` gives its displayed coverage.

Viewing Timeline Traces
-----------------------

To load traces into Chromium's ``chrome://tracing`` or Perfetto_, convert them
to Chrome trace format:

.. code:: console

   $ dune trace cat --chrome-trace > trace.json

.. _Perfetto: https://ui.perfetto.dev/
