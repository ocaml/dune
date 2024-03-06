map_workspace_root
-------------------

.. describe:: (map_workspace_root <bool>)

   Control references to the file system locations where the project has been
   built.

   - with ``(map_workspace_root true)``, dune rewrites references to the
     workspace root to ``/workspace_root``. Note that when this mapping is
     enabled, the debug information produced by the bytecode compiler is
     incorrect, as the location information is lost.

   - with ``(map_workspace_root false)``, the references are not rewritten.

   The default is ``(map_workspace_root true)``.

   .. versionadded:: 3.0
        Initial version with the mapping always enabled.
   .. versionchanged:: 3.7
        Add a way to disable the mapping.
