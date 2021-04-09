  $ mkdir -p partv2/partv1
  $ mkdir -p partv2/partv1bis
  $ cat >partv2/partv1/dune.inc <<EOF
  > ; comment top
  > (alias
  >  (name "runtest")
  >  (action (run %{bin:ocamlformat} -n 1 -i dir1/dir2/ignore_2.ml)))
  > 
  > (executable
  >  (name aux)
  >  (c_names cn1 cn2)
  >  (c_flags cf1 cf2)
  >  (cxx_names cxxn1 cxxn2)
  >  (cxx_flags cxxf1 cxxf2))
  > EOF

  $ cat >partv2/partv1/dune-project <<EOF
  > (lang dune 1.0)
  > (name pouet)
  > EOF

  $ cat >partv2/partv1/dune <<EOF
  > (include dune.inc)
  > 
  > (alias
  >  (name runtest)
  >  (action (run %{bin:ocamlformat} -n 1 -i dir1/dir2/ignore_2.ml)))
  > 
  > ; comment middle
  > (alias
  >  (action (run %{bin:ocamlformat} -n 1 -i dir1/dir2/ignore_2.ml))
  >  (name runtest)) ; comment end of line
  > 
  > (alias
  >  (name foo)
  >  (deps opam))
  > 
  > (executable
  >  (preprocessor_deps (alias foo))
  >  (modes exe))
  > 
  > (executable
  >  (preprocess no_preprocessing)
  >  (preprocessor_deps (alias foo)))
  > 
  > (executable
  >  (preprocess future_syntax)
  >  (preprocessor_deps (alias foo))
  >  (name toto))
  > 
  > (library
  >  (name foolib1)
  >  (no_keep_locs)
  >  (preprocess future_syntax)
  >  (preprocessor_deps (alias foo)))
  > 
  > (library
  >  (name foolib2)
  >  (preprocessor_deps (alias foo)))
  > EOF

  $ cat >partv2/partv1bis/dune-project <<EOF
  > (lang dune 1.11)
  > (name pouetbis)
  > (using fmt 1.2 (enabled_for reason))
  > EOF

  $ dune upgrade
  Project in dir partv2/partv1 will be upgraded to dune v2.
  Project in dir partv2/partv1bis will be upgraded to dune v2.
  Upgrading partv2/partv1bis/dune-project...
  Upgrading partv2/partv1/dune.inc...
  Upgrading partv2/partv1/dune...
  Upgrading partv2/partv1/dune-project...
  
  Some projects were upgraded to dune v2. Some breaking changes may not
  have been treated automatically. Here is a list of things you should check
  to complete the migration:
  
  - If you use generated dune.inc files you probably should update your
  generators.
  - mli only modules must now be explicitly declared. This was previously a
    warning and is now an error.
  - Stop installing the `ocaml-syntax-shims` binary. In order to use
    `future_syntax`, one now need to depend on the `ocaml-syntax-shims`
    package.
  - Actions which introduce targets where new targets are forbidden (e.g.
    preprocessing) are now an error instead of a warning.
  - Stop installing the `ocaml-syntax-shims` binary. In order to use
    `future_syntax`, one now need to depend on the `ocaml-syntax-shims`
    package.
  - Do not put the `<package>.install` files in the source tree unless `-p` or
    `--promote-install-files` is passed on the command line
  - Library names are now validated in a strict fashion. Previously, invalid
  names
    would be allowed for unwrapped libraries
  - Stricter validation of file names in `select`. The file names of
  conditional
    sources must match the prefix and the extension of the resultant filename.
  - Modules filtered out from the module list via the Ordered Set Language must
    now be actual modules.
  - Stub names are no longer allowed relative paths. This was previously a
  warning
    and is now an error.
  - In `(diff? x y)` action, require `x` to exist and register a
    dependency on that file.
  - `self_build_stubs_archive` was deleted in version 2.0 of the dune
  language. Use the (foreign_archives ...) field instead.

v1 -> v2

  $ cat partv2/partv1/dune-project
  (lang dune 2.0)
  
  (name pouet)
  
  (formatting disabled)
  $ cat partv2/partv1/dune
  (include dune.inc)
  
  (rule
   (alias runtest)
   (action
    (run %{bin:ocamlformat} -n 1 -i dir1/dir2/ignore_2.ml)))
  
  ; comment middle
  
  (rule
   (action
    (run %{bin:ocamlformat} -n 1 -i dir1/dir2/ignore_2.ml))
   (alias runtest))
  
  ; comment end of line
  
  (alias
   (name foo)
   (deps opam))
  
  (executable
   (modes exe))
  
  (executable
   (modes byte exe)
   (preprocess no_preprocessing))
  
  (executable
   (modes byte exe)
   (preprocess future_syntax)
   (preprocessor_deps
    (alias foo))
   (name toto))
  
  (library
   (name foolib1)
   (preprocess future_syntax)
   (preprocessor_deps
    (alias foo)))
  
  (library
   (name foolib2))
  $ cat partv2/partv1/dune.inc
  ; comment top
  
  (rule
   (alias "runtest")
   (action
    (run %{bin:ocamlformat} -n 1 -i dir1/dir2/ignore_2.ml)))
  
  (executable
   (foreign_stubs
    (language cxx)
    (names cxxn1 cxxn2)
    (flags cxxf1 cxxf2))
   (foreign_stubs
    (language c)
    (names cn1 cn2)
    (flags cf1 cf2))
   (modes byte exe)
   (name aux))

  $ cat partv2/partv1bis/dune-project
  (lang dune 2.0)
  
  (name pouetbis)
  
  (formatting
   (enabled_for reason))

Check that the upgrader is idempotent:
  $ dune upgrade
