# OxCaml package set — builds packages from the oxcaml opam repository.
{
  pkgs,
  lib,
  oxcamlOpamRepo,
}:

let
  packagesDir = "${oxcamlOpamRepo}/packages";
in
import ./opam-overlay.nix {
  inherit pkgs lib packagesDir;

  virtualPackages = [
    "ocaml"
    "dune"
    "ocaml-variants"
    "ocaml-options-vanilla"
    "ocaml-base-compiler"
    "base-unix"
    "base-bigarray"
    "base-threads"
    "base-domains"
    "base-nnp"
  ]
  ++ (builtins.filter (n: lib.hasPrefix "conf-" n) (
    builtins.attrNames (builtins.readDir packagesDir)
  ));

  nonDunePackages = [
    "obuild"
    "ocamlbuild"
    "ocamlfind"
    "topkg"
    "zarith"
  ];

  skipPackages = [
    "mdx"
    "odoc"
    "odoc-parser"
    "uutf"
  ];

  nameMap = {
    "ocamlfind" = "findlib";
  };
}
