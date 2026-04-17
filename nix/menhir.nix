# Menhir 20231231 — pinned version required by OxCaml.
{
  pkgs,
  menhir-src,
}:

let
  ocamlPackages = pkgs.ocamlPackages.overrideScope (
    oself: osuper: {
      menhirLib = osuper.menhirLib.overrideAttrs (old: {
        version = "20231231";
        src = menhir-src;
        patches = [ ];
      });
      menhirGLR = null;
      menhir = osuper.menhir.overrideAttrs (old: {
        version = "20231231";
        src = menhir-src;
        patches = [ ];
        buildInputs = builtins.filter (x: x != null) (old.buildInputs or [ ]);
        postInstall = (old.postInstall or "") + ''
          mkdir -p $out/lib/menhirLib
          cp ${oself.menhirLib}/lib/ocaml/*/site-lib/menhirLib/menhirLib.{ml,mli} $out/lib/menhirLib/
        '';
      });
    }
  );
in
{
  inherit (ocamlPackages) menhir menhirLib;
}
