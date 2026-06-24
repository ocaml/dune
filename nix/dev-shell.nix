# Returns the `makeDuneDevShell` helper, parameterised by the inputs that
# vary per evaluation system. The helper itself takes per-shell options and
# returns a `pkgs.mkShell` derivation.
{
  pkgs,
  INSIDE_NIX,
  testBuildInputs,
  testNativeBuildInputs,
  docInputs,
  sourceDune,
}:

{
  extraBuildInputs ? (pkgs: [ ]),
  meta ? null,
  duneFromScope ? false,
  includeTestDeps ? true,
  excludeTestNativeBuildInputs ? [ ],
  excludeOcamlLibs ? [ ],
  packageOverrides ? (oself: osuper: { }),
}:
let
  hasOcamlOverride = (packageOverrides { } { ocaml = null; }) ? ocaml;

  pkgs' =
    if hasOcamlOverride then
      pkgs.extend (
        pself: psuper: {
          ocamlPackages = psuper.ocamlPackages.overrideScope (
            oself: osuper:
            (pkgs.lib.mapAttrs (
              name: pkg:
              if pkgs.lib.isDerivation pkg && pkg ? overrideAttrs then
                pkg.overrideAttrs (old: {
                  doCheck = false;
                })
              else
                pkg
            ) osuper)
            // (packageOverrides oself osuper)
          );
        }
      )
    else if duneFromScope then
      pkgs.extend (
        pself: psuper: {
          ocamlPackages = psuper.ocamlPackages.overrideScope (
            oself: osuper: with oself; {
              dune_3 = sourceDune;
              fs-io = buildDunePackage {
                pname = "fs-io";
                inherit (dune_3) src version;
              };
              top-closure = buildDunePackage {
                pname = "top-closure";
                inherit (dune_3) src version;
              };
              dune-glob = osuper.dune-glob.overrideAttrs (o: {
                propagatedBuildInputs = o.propagatedBuildInputs ++ [
                  pp
                  re
                ];
              });
              stdune = osuper.stdune.overrideAttrs (o: {
                propagatedBuildInputs = o.propagatedBuildInputs ++ [
                  pp
                  fs-io
                  top-closure
                ];
              });
            }
          );
        }
      )
    else
      pkgs;

  inherit (pkgs') writeScriptBin stdenv;

  duneScript = writeScriptBin "dune" ''
    #!${stdenv.shell}
    "$DUNE_SOURCE_ROOT"/_boot/dune.exe $@
  '';

  baseInputs =
    if includeTestDeps then
      (builtins.filter (p: !builtins.elem (p.pname or p.name or "") excludeTestNativeBuildInputs) (
        testNativeBuildInputs pkgs'
      ))
      ++ docInputs
    else
      [ ];

  ocamlLibs =
    if includeTestDeps then
      builtins.filter (p: !builtins.elem (p.pname or p.name or "") excludeOcamlLibs) (
        with pkgs'.ocamlPackages;
        [
          ctypes
          cinaps
          integers
          lwt
          mdx
          menhir
          merlin
          ocaml-index
          ocaml-lsp
          odoc
          patdiff
          pp
          ppx_expect
          re
          spawn
          uutf
        ]
      )
    else
      [ ];
in

pkgs'.mkShell {
  shellHook = ''
    export DUNE_SOURCE_ROOT=$PWD
  '';
  inherit meta;
  nativeBuildInputs =
    baseInputs
    ++ pkgs'.lib.optionals stdenv.isDarwin [ pkgs'.darwin.sigtool ]
    ++ [ duneScript ]
    ++ (if hasOcamlOverride then [ pkgs'.ocamlPackages.ocaml ] else [ ]);
  inputsFrom = if hasOcamlOverride then [ ] else [ pkgs'.ocamlPackages.dune_3 ];
  buildInputs =
    (if includeTestDeps then testBuildInputs else [ ])
    ++ ocamlLibs
    ++ (extraBuildInputs pkgs')
    ++ (if hasOcamlOverride then [ pkgs'.ocamlPackages.findlib ] else [ ]);
  inherit INSIDE_NIX;
  dontDetectOcamlConflicts = hasOcamlOverride;
}
