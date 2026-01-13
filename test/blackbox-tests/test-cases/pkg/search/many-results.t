Load search/helpers.sh for helper to create many packages

We setup a simple mock repository directory with a couple of packages.

  $ mkrepo

Create multiple packages in the mock repo

  $ mk_multiple_packages

Create a dune-workspace with a the mock repository added

  $ add_mock_repo_if_needed

Search for all packages

  $ dune pkg search
  - alcotest 3.0~alpha1 This is just a very very very very very long synopsis
    for the excellent package: 'alcotest' version: '3.0~alpha1'
  - atdgen 3.0~alpha1 This is just a very very very very very long synopsis for
    the excellent package: 'atdgen' version: '3.0~alpha1'
  - base 3.0~alpha1 Short synopsis for package: 'base' version: '3.0~alpha1'
  - bos 3.0~alpha1 Short synopsis for package: 'bos' version: '3.0~alpha1'
  - cmdliner 3.0~alpha1 This is just a very very very very very long synopsis
    for the excellent package: 'cmdliner' version: '3.0~alpha1'
  - core 3.0~alpha1 Short synopsis for package: 'core' version: '3.0~alpha1'
  - dune 3.0~alpha1 Short synopsis for package: 'dune' version: '3.0~alpha1'
  - eio 3.0~alpha1 Short synopsis for package: 'eio' version: '3.0~alpha1'
  - js_of_ocaml 3.0~alpha1 This is just a very very very very very long
    synopsis for the excellent package: 'js_of_ocaml' version: '3.0~alpha1'
  - merlin 3.0~alpha1 This is just a very very very very very long synopsis for
    the excellent package: 'merlin' version: '3.0~alpha1'
  - notty 3.0~alpha1 This is just a very very very very very long synopsis for
    the excellent package: 'notty' version: '3.0~alpha1'
  - ocamlformat 3.0~alpha1 This is just a very very very very very long
    synopsis for the excellent package: 'ocamlformat' version: '3.0~alpha1'
  - owl 3.0~alpha1 Short synopsis for package: 'owl' version: '3.0~alpha1'
  - ppxlib 3.0~alpha1 This is just a very very very very very long synopsis for
    the excellent package: 'ppxlib' version: '3.0~alpha1'
  - re 3.0~alpha1 Short synopsis for package: 're' version: '3.0~alpha1'
  - reason 3.0~alpha1 This is just a very very very very very long synopsis for
    the excellent package: 'reason' version: '3.0~alpha1'
  - spectre 3.0~alpha1 This is just a very very very very very long synopsis
    for the excellent package: 'spectre' version: '3.0~alpha1'
  - utop 3.0~alpha1 Short synopsis for package: 'utop' version: '3.0~alpha1'
  - yojson 3.0~alpha1 This is just a very very very very very long synopsis for
    the excellent package: 'yojson' version: '3.0~alpha1'

Search with a query argument -- exact name

  $ dune pkg search dune
  - dune 3.0~alpha1 Short synopsis for package: 'dune' version: '3.0~alpha1'

Another search with a query argument - substring of package name.

  $ dune pkg search 'o'
  - alcotest 3.0~alpha1 This is just a very very very very very long synopsis
    for the excellent package: 'alcotest' version: '3.0~alpha1'
  - bos 3.0~alpha1 Short synopsis for package: 'bos' version: '3.0~alpha1'
  - core 3.0~alpha1 Short synopsis for package: 'core' version: '3.0~alpha1'
  - eio 3.0~alpha1 Short synopsis for package: 'eio' version: '3.0~alpha1'
  - js_of_ocaml 3.0~alpha1 This is just a very very very very very long
    synopsis for the excellent package: 'js_of_ocaml' version: '3.0~alpha1'
  - notty 3.0~alpha1 This is just a very very very very very long synopsis for
    the excellent package: 'notty' version: '3.0~alpha1'
  - ocamlformat 3.0~alpha1 This is just a very very very very very long
    synopsis for the excellent package: 'ocamlformat' version: '3.0~alpha1'
  - owl 3.0~alpha1 Short synopsis for package: 'owl' version: '3.0~alpha1'
  - reason 3.0~alpha1 This is just a very very very very very long synopsis for
    the excellent package: 'reason' version: '3.0~alpha1'
  - utop 3.0~alpha1 Short synopsis for package: 'utop' version: '3.0~alpha1'
  - yojson 3.0~alpha1 This is just a very very very very very long synopsis for
    the excellent package: 'yojson' version: '3.0~alpha1'

Another search with a query argument - substring of package name.

  $ dune pkg search 're'
  - re 3.0~alpha1 Short synopsis for package: 're' version: '3.0~alpha1'
  - core 3.0~alpha1 Short synopsis for package: 'core' version: '3.0~alpha1'
  - reason 3.0~alpha1 This is just a very very very very very long synopsis for
    the excellent package: 'reason' version: '3.0~alpha1'
  - spectre 3.0~alpha1 This is just a very very very very very long synopsis
    for the excellent package: 'spectre' version: '3.0~alpha1'
