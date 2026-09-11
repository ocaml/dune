display
-------

Specify the amount of Dune’s verbosity.

.. code:: dune

    (display <setting>)

where ``<setting>`` is one of:

- ``progress``, Dune shows and updates a status line as build goals are being
  completed. This is the default value.

- ``verbose`` prints the full command lines of programs being executed by Dune,
  with some colors to help differentiate programs.

- ``short`` prints a line for each program executed with the binary name on the
  left and the targets of the action on the right.

- ``quiet`` only display errors.

Progress Status Line
~~~~~~~~~~~~~~~~~~~~

In ``progress`` mode, a running build displays a line similar to:

.. code:: text

   Done: 75% (3/4, 1 left) (jobs: 1) | [1.2s] [0.8x] [2] | [rpc 1]

Its components are:

- ``Done: 75% (3/4, 1 left)`` reports the percentage and number of
  build rules completed, the total number discovered so far, and the number
  remaining. The total can increase as Dune discovers rules, and rules that
  are already up to date still count as completed. A failure count is added
  when a rule fails.

- ``jobs: 1`` is the number of jobs currently running.

- ``[1.2s]`` is the elapsed time for the current build. In watch mode, it is
  the duration of the most recently completed build while Dune waits for
  changes.

- ``[0.8x]`` is the average process parallelism: the accumulated CPU time of
  build processes divided by the elapsed build time. On Windows, where process
  CPU time is unavailable, Dune uses accumulated process wall-clock time.

- ``[2]`` is the current build number in watch mode, starting at 1.

- ``[rpc 1]`` is the number of RPC clients currently connected to this Dune
  process. It is shown only while at least one client is connected.

Bracketed components are displayed only when applicable. When a command sends
its request to an existing watch-mode Dune process, its own status line displays
``Connected to RPC server``; the detailed build status remains in the server's
output and is also available through ``dune monitor``.
