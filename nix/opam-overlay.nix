# Builds an ocamlPackages overlay from an opam repository.
#
# Given a packagesDir (e.g. "${opamRepo}/packages"), auto-discovers packages,
# parses their sources/patches/dependencies, and builds them with buildDunePackage.
#
# Usage:
#   overlay = import ./opam-overlay.nix {
#     inherit pkgs lib;
#     packagesDir = "${opamRepo}/packages";
#     virtualPackages = [ "ocaml" "dune" ... ];
#     nonDunePackages = [ "ocamlfind" ... ];
#     skipPackages = [ "uutf" ... ];
#     nameMap = { "ocamlfind" = "findlib"; };
#   };
#   # overlay : oself -> osuper -> { ... }
#
# - virtualPackages: provided externally (compiler, build system) — excluded from deps and builds
# - nonDunePackages: not built with dune — override nixpkgs with source+patches from the repo
# - skipPackages: use nixpkgs version as-is (not overridden)
# - nameMap: opam name -> nix attr name (only where they differ)
{
  pkgs,
  lib,
  packagesDir,
  virtualPackages,
  nonDunePackages,
  skipPackages,
  nameMap,
}:

let
  # --- Opam parsing ---

  extractQuoted =
    line:
    if line == null then null else
    let
      parts = builtins.match ''[^"]*"([^"\\]+)".*'' line;
    in
    if parts == null then
      throw "unsupported quoted opam string: ${line}"
    else
      builtins.head parts;

  extractBlock =
    startMarker: lines:
    let
      go =
        state: lines:
        if lines == [ ] then
          state.result
        else
          let
            line = builtins.head lines;
            rest = builtins.tail lines;
          in
          if state.inBlock then
            if lib.hasPrefix "]" line || lib.hasPrefix "}" line then
              state.result
            else
              go (state // { result = state.result ++ [ line ]; }) rest
          else if lib.hasInfix startMarker line then
            go (state // { inBlock = true; }) rest
          else
            go state rest;
    in
    go {
      inBlock = false;
      result = [ ];
    } lines;

  readOpamLines =
    pkgName: version:
    map lib.trim (
      lib.splitString "\n"
        (builtins.readFile "${packagesDir}/${pkgName}/${version}/opam")
    );

  enterSubpath =
    pkgName: version:
    let
      line = lib.findFirst (lib.hasPrefix "x-subpath:") null (
        readOpamLines pkgName version
      );
      subpath = extractQuoted line;
    in
    lib.optionalString (subpath != null) ''
      sourceRoot="$sourceRoot"/${lib.escapeShellArg subpath}
    '';

  getPatches =
    pkgName: version:
    let
      filesDir = "${packagesDir}/${pkgName}/${version}/files";
      files =
        if builtins.pathExists filesDir then builtins.attrNames (builtins.readDir filesDir) else [ ];
    in
    map (f: "${filesDir}/${f}") (builtins.filter (f: lib.hasSuffix ".patch" f) files);

  getSource =
    pkgName: version:
    let
      urlBlock = extractBlock "url {" (readOpamLines pkgName version);
      urlLine = lib.findFirst (l: lib.hasInfix "https://" l || lib.hasInfix "http://" l) null urlBlock;
      url = extractQuoted urlLine;
      sha256Line = lib.findFirst (l: lib.hasInfix "sha256=" l) null urlBlock;
      sha512Line = lib.findFirst (l: lib.hasInfix "sha512=" l) null urlBlock;
      sha256raw = extractQuoted sha256Line;
      sha512raw = extractQuoted sha512Line;
      hash =
        if sha256raw != null then "sha256:${lib.removePrefix "sha256=" sha256raw}"
        else if sha512raw != null then "sha512:${lib.removePrefix "sha512=" sha512raw}"
        else null;
    in
    if url != null && hash != null then pkgs.fetchurl { inherit url hash; } else null;

  parseDeps =
    pkgName: version:
    let
      depsBlock = extractBlock "depends:" (readOpamLines pkgName version);
      extractDepName = line:
        let m = builtins.match ''.*"([a-zA-Z0-9_-]+)".*'' line;
        in if m != null then builtins.head m else null;
      isDepLine = l:
        l != "" && l != "[" && l != "]"
        && !(lib.hasInfix "with-test" l)
        && !(lib.hasInfix "with-doc" l)
        && !(lib.hasInfix "with-dev-setup" l);
      isBuildDep = l:
        lib.hasInfix "{build}" l || lib.hasInfix "{build " l || lib.hasInfix "{build&" l;
      allDepLines = builtins.filter isDepLine depsBlock;
    in
    {
      buildDeps = builtins.filter (x: x != null) (map extractDepName (builtins.filter isBuildDep allDepLines));
      runtimeDeps = builtins.filter (x: x != null) (map extractDepName (builtins.filter (l: !isBuildDep l) allDepLines));
    };

  # --- Discovery ---

  discovered = builtins.listToAttrs (
    map (name:
      let
        versions = builtins.filter
          (version: lib.hasPrefix "${name}." version)
          (builtins.attrNames (builtins.readDir "${packagesDir}/${name}"));
        preferredVersions = builtins.filter
          (version:
            !lib.hasInfix "flags: avoid-version"
              (builtins.readFile "${packagesDir}/${name}/${version}/opam"))
          versions;
        selectableVersions =
          if preferredVersions == [ ] then versions else preferredVersions;
        sorted = builtins.sort
          (a: b: builtins.compareVersions a b < 0)
          selectableVersions;
      in
      { inherit name; value = lib.last sorted; }
    ) (builtins.attrNames (builtins.readDir packagesDir))
  );

  # --- Building ---

  nixAttrName = opamName: nameMap.${opamName} or opamName;

  resolveDeps =
    oself: depNames:
    builtins.filter (d: d != null) (
      map (d: oself.${nixAttrName d} or null) (
        builtins.filter (d: !(builtins.elem d virtualPackages)) depNames
      )
    );

  mkPackage =
    oself: pkgName: versionDir:
    let
      src = getSource pkgName versionDir;
      patches = getPatches pkgName versionDir;
      deps = parseDeps pkgName versionDir;
    in
    oself.buildDunePackage {
      pname = pkgName;
      version = lib.removePrefix "${pkgName}." versionDir;
      inherit src patches;
      postUnpack = enterSubpath pkgName versionDir;
      DUNE_ROOT = ".";
      nativeBuildInputs = resolveDeps oself deps.buildDeps;
      propagatedBuildInputs = resolveDeps oself deps.runtimeDeps;
      preBuild = ''
        patchShebangs .
      '';
      doCheck = false;
    };

  applyNonDuneOverride =
    osuper: nixName: ver:
    let
      existing = osuper.${nixName} or null;
      opamName = lib.findFirst (n: nixAttrName n == nixName) nixName nonDunePackages;
      src = getSource opamName ver;
      patches = getPatches opamName ver;
    in
    if existing != null && lib.isDerivation existing then
      existing.overrideAttrs (old:
        { doCheck = false; }
        // (lib.optionalAttrs (src != null) {
          inherit src;
          postUnpack =
            (old.postUnpack or "") + "\n" + enterSubpath opamName ver;
        })
        // (lib.optionalAttrs (patches != [ ]) {
          patches = (old.patches or [ ]) ++ patches;
        })
      )
    else
      existing;

  excludedPackages = virtualPackages ++ nonDunePackages ++ skipPackages;

  dunePackageNames = builtins.filter
    (name: !(builtins.elem name excludedPackages))
    (builtins.attrNames (builtins.readDir packagesDir));

  nonDuneOverrides = builtins.listToAttrs (
    builtins.filter (x: x != null) (
      map (name:
        if discovered ? ${name} then
          { name = nixAttrName name; value = discovered.${name}; }
        else
          null
      ) nonDunePackages
    )
  );

in
# The overlay: oself -> osuper -> { ... }
oself: osuper:
let
  autoBuilt = builtins.listToAttrs (
    map (name: {
      name = nixAttrName name;
      value = mkPackage oself name discovered.${name};
    }) dunePackageNames
  );

  nonDuneBuilt = lib.mapAttrs
    (name: ver: applyNonDuneOverride osuper name ver)
    nonDuneOverrides;
in
autoBuilt // nonDuneBuilt
