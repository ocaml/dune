1.0+beta10 (in development)
---------------------------

- Add a `clean` subcommand (Richard Davison, #89)

- Fix a bug where `jbuild rules` would crash instead of reporting a
  proper build error

- Don't use unix in the bootstrap script, to avoid surprises with
  Cygwin

1.0+beta9 (19/05/2017)
----------------------

- Add support for building Reason projects (Rudi Grinberg, #58)

- Add support for building javascript with js-of-ocaml (Hugo Heuzard,
  #60)

- Better support for topkg release workflow. See
  [topkg-jbuilder](https://github.com/diml/topkg-jbuilder) for more
  details

- Port the manual to rst and setup a jbuilder project on
  readthedocs.org (Rudi Grinberg, #78)

- Hint for mistyped targets. Only suggest correction on the basename
  for now, otherwise it's slow when the workspace is big

- Add a `(package ...)` field for aliases, so that one can restrict
  tests to a specific package (Rudi Grinberg, #64)

- Fix a couple of bugs on Windows:
  + fix parsing of end of lines in some cases
  + do not take the case into account when comparing environment
    variable names

- Add AppVeyor CI

- Better error message in case a chain of dependencies *crosses* the
  installed world

- Better error messages for invalid dependency list in jbuild files

- Severel improvements/fixes regarding the handling of findlib packages:
  + Better error messages when a findlib package is unavailable
  + Don't crash when an installed findlib package has missing
    dependencies
  + Handle the findlib alternative directory layout which is still
    used by a few packages

- Add `jbuilder installed-libraries --not-available` explaining why
  some libraries are not available

- jbuilder now records dependencies on files of external
  libraries. This mean that when you upgrade a library, jbuilder will
  know what need to be rebuilt.

- Add a `jbuilder rules` subcommand to dump internal compilation
  rules, mostly for debugging purposes

- Ignore all directories starting with a `.` or `_`. This seems to be
  a common pattern:
  - `.git`, `.hg`, `_darcs`
  - `_build`
  - `_opam` (opam 2 local switches)

- Fix the hint for `jbuilder external-lib-deps` (#72)

- Do not require `ocamllex` and `ocamlyacc` to be at the same location
  as `ocamlc` (#75)

1.0+beta8 (17/04/2017)
----------------------

- Added `${lib-available:<library-name>}` which expands to `true` or
  `false` with the same semantic as literals in `(select ...)` stanzas

- Remove hard-coded knowledge of a few specific ppx rewriters to ease
  maintenance moving forward

- Pass the library name to ppx rewriters via the `library-name` cookie

- Fix: make sure the action working directory exist before running it

1.0+beta7 (12/04/2017)
----------------------

- Make the output quieter by default and add a `--verbose` argument
  (Stephen Dolan, #40)

- Various documentation fixes (Adrien Guatto, #41)

- Make `@install` the default target when no targets are specified
  (Stephen Dolan, #47)

- Add predefined support for menhir, similar to ocamlyacc support
  (Rudi Grinberg, #42)

- Add internal support for sandboxing actions and sandbox the build of
  the alias module with 4.02 to workaround the compiler trying to read
  the cmi of the aliased modules

- Allow to disable dynlink support for libraries via `(no_dynlink)`
  (#55)

- Add a -p/--for-release-of-packages command line argument to simplify
  the jbuilder invocation in opam files and make it more future proof
  (#52)

- Fix the lookup of the executable in `jbuilder exec foo`. Before,
  even if `foo` was to be installed, the freshly built version wasn't
  selected

- Don't generate a `exists_if ...` lines in META files. These are
  useless sine the META files are auto-generated

1.0+beta6 (29/03/2017)
----------------------

- Add an `(executable ...)` stanza for single executables (#33)

- Add a `(package ...)` and `(public_name <name>)/(public_names
   (<names))` to `executable/executables` stanzas to make it easier to
  install executables (#33)

- Fix a bug when using specific rewriters that jbuilder knows about
  without `ppx_driver.runner` (#37). These problem should go away
  soon when we start using `--cookie`

- Fix the interpretation of META files when there is more than one
  applicable assignment. Before this fix, the one with the lowest
  number of formal predicates was selected instead of the one with the
  biggest number of formal predicates

1.0+beta5 (22/03/2017)
----------------------

- When `ocamlfind` is present in the `PATH`, do not attempt to call
  `opam config var lib`

- Make sure the build of jbuilder itself never calls `ocamlfind` or
  `opam`

- Better error message when a jbuild file in OCaml syntax forgets to
  call `Jbuild_plugin.V*.send`

- Added examples of use

- Don't drop inline tests/benchmarks by default

1.0+beta4 (20/03/2017)
----------------------

- Improve error messages about invalid/missing pkg.opam files

- Ignore all errors while running `ocamlfind printconf path`

1.0+beta3 (15/03/2017)
----------------------

- Print optional dependencies as optional in the output of `jbuilder
   external-lib-deps --missing`

- Added a few forms to the DSL:
  - `with-{stderr,outputs}-to`
  - `ignore-{stdout,stderr,outputs}`
- Added `${null}` which expands to `/dev/null` on Unix and `NUL` on
  Windows

- Improve the doc generated by `odoc` for wrapped libraries

- Improve the error reported when an installed package depends on a
  library that is not installed

- Documented `(files_recursively_in ...)`

- Added black box tests

- Fix a bug where `jbuilder` would crash when there was no
  `<package>.opam` file

- Fixed a bug where `.merlin` files where not generated at the root of
  the workspace (#20)

- Fix a bug where a `(glob_files ...)` would cause other dependencies
  to be ignored

- Fix the generated `ppx(...)` line in `META` files

- Fix `(optional)` when a ppx runtime dependency is not available
  (#24)

- Do not crash when an installed package that we don't need has
  missing dependencies (#25)

1.0+beta2 (10/03/2017)
----------------------

- Simplified the rules for finding the root of the workspace as the
  old ones were often picking up the home directory. New rules are:
  + look for a `jbuild-workspace` file in parent directories
  + look for a `jbuild-workspace*` file in parent directories
  + use the current directory
- Fixed the expansion of `${ROOT}` in actions

- Install `quick-start.org` in the documentation directory

- Add a few more things in the log file to help debugging

1.0+beta1 (07/03/2017)
----------------------

- Added a manual

- Support incremental compilation

- Switched the CLI to cmdliner and added a `build` command (#5, Rudi
  Grinberg)

- Added a few commands:
  + `runtest`
  + `install`
  + `uninstall`
  + `installed-libraries`
  + `exec`: execute a command in an environment similar to what you
    would get after `jbuilder install`
- Removed the `build-package` command in favor of a `--only-packages`
  option that is common to all commands

- Automatically generate `.merlin` files (#2, Richard Davison)

- Improve the output of jbuilder, in particular don't mangle the
  output of commands when using `-j N` with `N > 1`

- Generate a log in `_build/log`

- Versioned the jbuild format and added a first stable version. You
  should now put `(jbuilder_version 1)` in a `jbuild` file at the root
  of your project to ensure forward compatibility

- Switch from `ppx_driver` to `ocaml-migrate-parsetree.driver`. In
  order to use ppx rewriters with Jbuilder, they need to use
  `ocaml-migrate-parsetree.driver`

- Added support for aliases (#7, Rudi Grinberg)

- Added support for compiling against multiple opam switch
  simultaneously by writing a `jbuild-worspace` file

- Added support for OCaml 4.02.3

- Added support for architectures that don't have natdynlink

- Search the root according to the rules described in the manual
  instead of always using the current directory

- extended the action language to support common actions without using
  a shell:
  + `(with-stdout-to <file> <DSL>)`
  + `(copy <src> <dst>)`
  + ...

- Removed all implicit uses of bash or the system shell. Now one has
  to write explicitely `(bash "...")` or `(system "...")`

- Generate meaningful versions in `META` files

- Strengthen the scope of a package. Jbuilder knows about package
  `foo` only in the sub-tree starting from where `foo.opam` lives

0.1.alpha1 (04/12/2016)
-----------------------

First release
