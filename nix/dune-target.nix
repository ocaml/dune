# Cross-compile-friendly dune derivation. Native: a buildDunePackage template
# (not buildable natively by design — exists so findlib config has dune's deps
# in scope). Cross: overrides build/install phases to use `dune build -x
# <target>`.
{
  lib,
  stdenv,
  buildPackages,
  buildDunePackage,
  ocaml,
  dune_3,
  csexp,
  pp,
  ppx_expect,
  re,
  spawn,
  uutf,
}:
let
  isCross = stdenv.hostPlatform.config != stdenv.buildPlatform.config;
  # Must match the toolchain name nix-overlays sets in the findlib config
  # (`cross/ocaml.nix`). Otherwise dune's `-x` lookup misses and falls back
  # to a native build.
  crossName =
    if stdenv.hostPlatform.isMinGW then "windows" else lib.head (lib.splitString "-" stdenv.system);
  installedName = if stdenv.hostPlatform.isMinGW then "dune.exe" else "dune";

  template = buildDunePackage {
    pname = "dune_target";
    inherit (dune_3) version src;
    buildInputs = [
      csexp
      pp
      ppx_expect
      re
      spawn
      uutf
    ];
  };
in
if !isCross then
  template
else
  assert stdenv.hostPlatform.isMinGW -> lib.versionAtLeast ocaml.version "5.4";
  template.overrideAttrs (o: {
    dontAddPrefix = true;
    dontFixup = true;
    depsBuildBuild = [ buildPackages.stdenv.cc ];
    nativeBuildInputs = lib.remove buildPackages.stdenv.cc (o.nativeBuildInputs or [ ]);
    buildPhase = ''
      runHook preBuild
      dune build -x ${crossName} ''${enableParallelBuilding:+-j $NIX_BUILD_CORES} bin/main.exe
      runHook postBuild
    '';
    installPhase = ''
      runHook preInstall
      mkdir -p $out/bin
      cp _build/default.${crossName}/bin/main.exe $out/bin/${installedName}
      runHook postInstall
    '';
  })
