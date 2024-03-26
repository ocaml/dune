action_stdout_on_success
------------------------

Specifies how Dune should handle the standard output of actions when they succeed.
This can be used to reduce the noise of large builds.

.. code:: dune

    (action_stdout_on_success <setting>)

where ``<setting>`` is one of:

- ``print`` prints the output on the terminal. This is the default.

- ``swallow`` ignores the output and does not print it on the terminal.

- ``must-be-empty`` enforces that the output should be empty. If it is not, Dune will fail.
