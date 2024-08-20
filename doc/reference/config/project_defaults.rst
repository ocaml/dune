project_defaults
----------------

Specifies default values for various stanzas in the generated ``dune-project``
file when using ``dune init project``. The format of the project default stanza
is as follows:

.. code:: dune

   (project_defaults
    <optional-fields>)

``<optional-fields`` are:

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

   Specify license.

   Example:

   .. code:: dune

       (project_defaults
        (license "MIT"))
