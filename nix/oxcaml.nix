# OxCaml setup for this flake.
#
# Three pins live here. They need to be coordinated when bumping
# OxCaml; eval-time failures drive the next step:
#
#   1. Bump `oxcaml-opam-repository.rev`.
#      → fail: narHash mismatch.
#   2. Update `oxcaml-opam-repository.narHash` to whatever nix prints.
#      Re-evaluate; the parsed `oxcamlTag` from the opam file may now
#      be a different tag.
#   3. Bump `oxcaml.rev` to the upstream OxCaml commit for that tag,
#      and update `oxcaml.narHash` to match. Resolve both via:
#        nix flake metadata --json github:oxcaml/oxcaml/<new-tag>
#
# `oxcamlTag` is parsed from the opam file purely so a reader can see
# the tag the rev below corresponds to, and so a `throw` fires if the
# pin gets out of sync with what the opam-repo expects.
{ pkgs }:

let
  lib = pkgs.lib;

  oxcaml-opam-repository = builtins.fetchTree {
    type = "github";
    owner = "oxcaml";
    repo = "opam-repository";
    rev = "c2662be9bcad3875554acccff7bd1e3fe8cb57bc";
    narHash = "sha256-2zn4PKRw7vsSh7tp+/nBZFjGE6fWrYm9lgrIWmvfVS8=";
  };

  # Parsed for documentation / sanity-check; not used in the getFlake
  # URL (pure mode needs rev, not tag).
  compilerPkgDir = "${oxcaml-opam-repository}/packages/oxcaml-compiler";
  compilerVersions = builtins.attrNames (builtins.readDir compilerPkgDir);
  preferredCompilerVersions =
    builtins.filter
      (version:
        !lib.hasInfix "flags: avoid-version"
          (builtins.readFile "${compilerPkgDir}/${version}/opam"))
      compilerVersions;
  selectableCompilerVersions =
    if preferredCompilerVersions == [ ] then
      compilerVersions
    else
      preferredCompilerVersions;
  compilerVersion = lib.last (lib.naturalSort selectableCompilerVersions);
  compilerOpamFile = builtins.readFile "${compilerPkgDir}/${compilerVersion}/opam";
  oxcamlTag =
    let
      match = builtins.match ''.*archive/refs/tags/([^"]+)\.tar\.gz.*'' compilerOpamFile;
    in
    if match == null then
      throw "could not parse OxCaml tag from ${compilerPkgDir}/${compilerVersion}/opam"
    else
      builtins.head match;

  # The upstream OxCaml flake at the commit corresponding to oxcamlTag.
  # See the file header for how to resolve rev+narHash.
  oxcaml = builtins.getFlake "github:oxcaml/oxcaml/dd4e8507373d22fb295422eb6dd3d997c76c47cb?narHash=sha256-IXgxNbNS5JlZeR3loZt2SRKrOI7hrOfC%2F%2FmQHGafhxY%3D";

  # Sanity: the tag in the opam-repo matches the tag this rev was
  # resolved from. Documentation more than enforcement — the assert
  # fires if a reader bumps one of the three pins above without the
  # others.
  expectedTag = "5.2.0minus-39";
  _tagOk =
    if oxcamlTag == expectedTag then
      null
    else
      throw "oxcaml-opam-repository expects tag '${oxcamlTag}' but the oxcaml pin in this file targets '${expectedTag}'. Update one or the other.";

  oxPackageSet = import ./ox-package-set.nix {
    inherit pkgs lib;
    oxcamlOpamRepo = oxcaml-opam-repository;
  };
in
{
  compiler = builtins.seq _tagOk (
    oxcaml.packages.${pkgs.stdenv.hostPlatform.system}.default.overrideAttrs (old: {
      NIX_CFLAGS_COMPILE = "-std=gnu17";
      passthru = (old.passthru or { }) // pkgs.ocamlPackages.ocaml.passthru;
      meta = (old.meta or { }) // pkgs.ocamlPackages.ocaml.meta;
    })
  );

  packageSet = oxPackageSet;
}
