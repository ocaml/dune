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
    rev = "231c88c2e564fdca40e15e750aacad5fb0887435";
    narHash = "sha256-+9ZQF2VH0O0G8RtN0hTf4k+w6yL63g6tblq3uzTKzG8=";
  };

  # Parsed for documentation / sanity-check; not used in the getFlake
  # URL (pure mode needs rev, not tag).
  compilerPkgDir = "${oxcaml-opam-repository}/packages/oxcaml-compiler";
  compilerVersion = lib.last (lib.naturalSort (builtins.attrNames (builtins.readDir compilerPkgDir)));
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
  oxcaml = builtins.getFlake "github:oxcaml/oxcaml/7d714cfb3f1c79c9b1e2a9c40ac60ba0c44cafd7?narHash=sha256-JwVNRca8q1WfmuFLPOK3hL1DxmaKRYd%2BgJgV9xTDUhI%3D";

  # Sanity: the tag in the opam-repo matches the tag this rev was
  # resolved from. Documentation more than enforcement — the assert
  # fires if a reader bumps one of the three pins above without the
  # others.
  expectedTag = "5.2.0minus-31";
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
