# Compiler and package pins shared by the Nix shells and CI.
# Resolve compiler revs and hashes with `nix flake prefetch --json`.
{ pkgs }:

let
  lib = pkgs.lib;
  pins = builtins.fromJSON (builtins.readFile ./oxcaml.json);

  oxcaml-opam-repository = builtins.fetchTree (
    {
      type = "github";
      owner = "oxcaml";
      repo = "opam-repository";
    }
    // pins.opamRepository
  );

  compilerOpamFile = builtins.readFile (
    "${oxcaml-opam-repository}/packages/oxcaml-compiler/"
    + "oxcaml-compiler.${pins.release.version}/opam"
  );
  oxcamlTag =
    let
      match = builtins.match ''.*archive/refs/tags/([^"]+)\.tar\.gz.*'' compilerOpamFile;
    in
    if match == null then
      throw "could not parse the OxCaml tag from the pinned opam package"
    else
      builtins.head match;

  compilerFor =
    pin:
    let
      oxcaml = builtins.getFlake ("github:oxcaml/oxcaml/${pin.rev}?narHash=${lib.escapeURL pin.narHash}");
    in
    oxcaml.packages.${pkgs.stdenv.hostPlatform.system}.default.overrideAttrs (old: {
      NIX_CFLAGS_COMPILE = "-std=gnu17";
      passthru = (old.passthru or { }) // pkgs.ocamlPackages.ocaml.passthru;
      meta = (old.meta or { }) // pkgs.ocamlPackages.ocaml.meta;
    });

  tagOk =
    if oxcamlTag == pins.release.tag then
      null
    else
      throw "oxcaml-opam-repository expects tag '${oxcamlTag}' but the compiler pin targets '${pins.release.tag}'";
in
{
  compiler = builtins.seq tagOk (compilerFor pins.release);
  latestCompiler = compilerFor pins.latest;

  packageSet = import ./ox-package-set.nix {
    inherit pkgs lib;
    oxcamlOpamRepo = oxcaml-opam-repository;
  };
}
