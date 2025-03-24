project_defaults
----------------

.. versionadded:: 3.17

Specify default values for stanzas ``authors``, ``maintainers``, and ``license``
of the :doc:`../dune-project/index` file when initializing a project with 
``dune init proj``. The format of the 'project_defaults' stanza is as follows:

.. code:: dune

   (project_defaults
    <optional-fields>)

``<optional-fields>`` are:

.. describe:: (authors <string(s)>)

   Specify authors.

   Example:

   .. code:: dune
        
       (project_defaults
        (authors
         "Jane Doe <jane.doe@example.com>"
         "John Doe <john.doe@example.com>"))

.. describe:: (maintainers <string(s)>)

   Specify maintainers.

   Example:

   .. code:: dune

       (project_defaults
        (maintainers
         "Jane Doe <jane.doe@example.com>"
         "John Doe <john.doe@example.com>"))

.. describe:: (license <string(s)>)

   Specify license, ideally as an identifier from the `SPDX License List
   <https://spdx.org/licenses/>`__.

   Example:

   .. code:: dune

       (project_defaults
        (license "MIT"))
