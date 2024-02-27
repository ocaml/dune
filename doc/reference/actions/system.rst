########
 system
########

.. highlight:: dune

.. dune:action:: system
   :param: <cmd>

   Execute a command using the system shell: ``sh`` on Unix and ``cmd``
   on Windows.

   Example:

   .. code::

      (system "command arg1 arg2")
