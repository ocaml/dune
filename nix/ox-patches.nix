{ pkgs, lib, oxcamlOpamRepo }:

let
  packagesDir = "${oxcamlOpamRepo}/packages";

  # Gets all patch files from a package version directory
  getPatchesFromDir = pkgName: version:
    let
      filesDir = "${packagesDir}/${pkgName}/${version}/files";
      files =
        if builtins.pathExists filesDir
        then builtins.attrNames (builtins.readDir filesDir)
        else [ ];
      patchFiles = lib.filter (f: lib.hasSuffix ".patch" f) files;
    in
    map (patchFile: "${filesDir}/${patchFile}") patchFiles;

  oxcamlPatches = {
    base = {
      patches = getPatchesFromDir "base" "base.v0.18~preview.130.55+197";
    };
    core = {
      patches = getPatchesFromDir "core" "core.v0.18~preview.130.55+197";
    };
    core_kernel = {
      patches = getPatchesFromDir "core_kernel" "core_kernel.v0.18~preview.130.55+197";
    };
    dune_3 = {
      patches = getPatchesFromDir "dune" "dune.3.20.2+ox";
    };
    ocaml-compiler-libs = {
      patches = getPatchesFromDir "ocaml-compiler-libs" "ocaml-compiler-libs.v0.17.0+ox";
    };
    ocamlbuild = {
      patches = getPatchesFromDir "ocamlbuild" "ocamlbuild.0.15.0+ox";
    };
    # ppxlib = {
    #   patches = getPatchesFromDir "ppxlib" "ppxlib.0.33.0+ox";
    # };
    topkg = {
      patches = getPatchesFromDir "topkg" "topkg.1.0.8+ox";
    };
  };

  # Applies patches to a package if configured
  applyPatchesIfExists = pkgName: pkg:
    if oxcamlPatches ? ${pkgName} && lib.isDerivation pkg && pkg ? overrideAttrs then
      let
        patchConfig = oxcamlPatches.${pkgName};
        numPatches = builtins.length patchConfig.patches;
      in
      if numPatches > 0 then
        pkg.overrideAttrs
          (old: {
            postPatch = (old.postPatch or "") + ''
              ${lib.concatMapStringsSep "\n" (patch: ''
                if patch -p1 --no-backup-if-mismatch < ${patch} 2>&1 | grep -q "FAILED"; then
                  echo "oxcaml: patch failed for ${pkgName}: ${builtins.baseNameOf patch}" >&2
                fi
              '') patchConfig.patches}
            '';
          })
      else
        pkg
    else
      pkg;

in
oself: osuper:
lib.mapAttrs
  (name: pkg:
  if lib.isDerivation pkg && pkg ? overrideAttrs
  then applyPatchesIfExists name pkg
  else pkg
  )
  osuper
