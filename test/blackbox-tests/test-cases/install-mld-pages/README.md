Since odoc 3, pages can be installed in subdirectories of `doc/odoc-pages/`, so
that odoc drivers (such as odig or the ocaml.org odoc driver) are able to
recover the hierarchy.

For `dune`, this means that we need to provide the user a way to define the
hierarchy they want for their mld files. Once the hierarchy is defined, `dune` needs to respect it by:

- Installing the files at the right location (for other drivers),
- Driving `odoc` according to the hierarchy

This directory contains tests for the first point.
