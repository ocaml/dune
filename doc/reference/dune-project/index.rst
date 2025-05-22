dune-project
============

Each ``dune-project`` file marks the root of a Dune projects within the current
Dune workspace (see :doc:`/getting-started/scopes`). It can also specify
project-wide parameters.

The first line of ``dune-project`` must be a ``lang`` stanza with no extra
whitespace or comments. The ``lang`` stanza controls the names and contents of
all configuration files read by Dune and looks like:

.. code:: dune

   (lang dune 3.14)

Additionally, they can contains the following stanzas.

.. toctree::

  accept_alternative_dune_file_name
  cram
  dialect
  executables_implicit_empty_intf
  expand_aliases_in_sandbox
  explicit_js_mode
  formatting
  generate_opam_files
  implicit_transitive_deps
  map_workspace_root
  name
  opam_file_location
  package
  pin
  subst
  use_standard_c_and_cxx_flags
  using
  version
  warnings
  wrapped_executables
