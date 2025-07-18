{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    melange = {
      url = "github:melange-re/melange/refs/tags/5.1.0-53";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    ocaml-overlays = {
      url = "github:nix-ocaml/nix-overlays";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    { self
    , flake-utils
    , nixpkgs
    , melange
    , ocaml-overlays
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system}.appendOverlays [
        ocaml-overlays.overlays.default
        (self: super: {
          ocamlPackages = super.ocaml-ng.ocamlPackages_5_3.overrideScope (oself: osuper: {
            mdx = osuper.mdx.override {
              logs = oself.logs;
            };
            utop = osuper.utop.overrideAttrs {
              dontGzipMan = true;
            };
          });
        })
        melange.overlays.default
        (self: super: {
          coq_8_16_native = super.coq_8_16.overrideAttrs (a: {
            configureFlags = [ "-native-compiler" "yes" ];
          });
        })
      ];
      dune-static-overlay = self: super: {
        ocamlPackages = super.ocaml-ng.ocamlPackages_5_3.overrideScope (oself: osuper: {
          dune_3 = osuper.dune_3.overrideAttrs (a: {
            src = ./.;
            preBuild = "ocaml boot/bootstrap.ml --static";
          });
        });
      };
      # This creates a git repo and creates an annotated tag named after the
      # current version of dune. This is necessary for the resulting dune
      # executable to print the correct version in the output of `dune
      # --version`. The git metadata is not copied to the environment where
      # this flake is built, so the current tag of the repo can't be known
      # here. Instead it's assumed that the first word of the first line of the
      # changelog will be the version number of the current release. The logic
      # in "dune subst" that determines the output of `dune --version`
      # determines the version number from git, so we need to create a git repo
      # containing the code with an annotated tag matching the desired version
      # number.
      version-tag-from-changelog = ''
        VERSION=$(head -n1 CHANGES.md | cut -f1 -d' ')
        export PATH=${pkgs.git}/bin:$PATH
        export GIT_COMMITTER_NAME=user GIT_COMMITTER_EMAIL=user@example.com GIT_AUTHOR_NAME=user GIT_AUTHOR_EMAIL=user@example.com
        git init
        git add .
        git commit --allow-empty -m dummy
        git tag $VERSION -am dummy
      '';
      dune-versioned-overlay = self: super: {
        ocamlPackages = super.ocaml-ng.ocamlPackages_5_3.overrideScope (oself: osuper: {
          dune_3 = osuper.dune_3.overrideAttrs (a: {
            src = ./.;
            preBuild = ''
              ${version-tag-from-changelog}
              ocaml boot/bootstrap.ml
              _boot/dune.exe subst
            '';
          });
        });
      };
      dune-static-versioned-overlay = self: super: {
        ocamlPackages = super.ocaml-ng.ocamlPackages_5_3.overrideScope (oself: osuper: {
          dune_3 = osuper.dune_3.overrideAttrs (a: {
            src = ./.;
            preBuild = ''
              ${version-tag-from-changelog}
              ocaml boot/bootstrap.ml --static
              _boot/dune.exe subst
            '';
          });
        });
      };
      pkgs-static = nixpkgs.legacyPackages.${system}.appendOverlays [
        ocaml-overlays.overlays.default
        dune-static-overlay
      ];
      pkgs-versioned = nixpkgs.legacyPackages.${system}.appendOverlays [
        ocaml-overlays.overlays.default
        dune-versioned-overlay
      ];
      pkgs-static-versioned = nixpkgs.legacyPackages.${system}.appendOverlays [
        ocaml-overlays.overlays.default
        dune-static-versioned-overlay
      ];

      add-experimental-configure-flags = pkg: pkg.overrideAttrs {
        configureFlags =
          [
            "--pkg-build-progress" "enable"
            "--lock-dev-tool" "enable"
            "--portable-lock-dir" "enable"
          ];
      };

      ocamlformat =
        let
          ocamlformat_version =
            let
              lists = pkgs.lib.lists;
              strings = pkgs.lib.strings;
              ocamlformat_config = strings.splitString "\n" (builtins.readFile ./.ocamlformat);
              prefix = "version=";
              ocamlformat_version_pred = line: strings.hasPrefix prefix line;
              version_line = lists.findFirst ocamlformat_version_pred "not_found" ocamlformat_config;
              version = strings.removePrefix prefix version_line;
            in
            builtins.replaceStrings [ "." ] [ "_" ] version;
        in
        builtins.getAttr ("ocamlformat_" + ocamlformat_version) pkgs;

      testBuildInputs = with pkgs;
        [ file mercurial unzip ]
        ++ lib.optionals stdenv.isLinux [ strace ];
      testNativeBuildInputs = pkgs: with pkgs; [
        nodejs-slim
        pkg-config
        opam
        ocamlformat
      ];

      docInputs = with pkgs.python3.pkgs; [
        sphinx-autobuild
        furo
        sphinx-copybutton
        sphinx-design
        myst-parser
      ];
    in
    {
      formatter = pkgs.nixpkgs-fmt;

      packages = {
        default = with pkgs; stdenv.mkDerivation {
          pname = "dune";
          version = "3.x-n/a";
          src =
            let fs = lib.fileset; in
            fs.toSource {
              root = ./.;
              fileset = fs.unions [
                ./bin
                ./boot
                ./configure
                ./dune-project
                ./dune-file
                ./src
                ./plugin
                ./vendor
                ./otherlibs
                ./Makefile
                (fs.fileFilter (file: file.hasExt "opam" || file.hasExt "template") ./.)
              ];
            };
          nativeBuildInputs = with ocamlPackages; [ ocaml findlib ];
          buildInputs = lib.optionals stdenv.isDarwin [
            darwin.apple_sdk.frameworks.CoreServices
          ];
          strictDeps = true;
          buildFlags = [ "release" ];
          dontAddPrefix = true;
          dontAddStaticConfigureFlags = true;
          configurePlatforms = [ ];
          installFlags = [ "PREFIX=${placeholder "out"}" "LIBDIR=$(OCAMLFIND_DESTDIR)" ];
        };
        dune = self.packages.${system}.default;
        dune-static = pkgs-static.pkgsCross.musl64.ocamlPackages.dune;
        dune-versioned = pkgs-versioned.ocamlPackages.dune;
        dune-static-versioned = pkgs-static-versioned.pkgsCross.musl64.ocamlPackages.dune;
        dune-experimental = add-experimental-configure-flags self.packages.${system}.dune;
        dune-static-experimental = add-experimental-configure-flags self.packages.${system}.dune-static;
      };

      devShells =
        let
          makeDuneDevShell =
            { extraBuildInputs ? (pkgs: [ ])
            , meta ? null
            , duneFromScope ? false
            }:
            let
              pkgs' =
                if duneFromScope then
                  pkgs.extend
                    (pself: psuper: {
                      ocamlPackages = psuper.ocamlPackages.overrideScope (oself: osuper: {
                        dune_3 = self.packages.${system}.default;
                      });
                    })
                else pkgs;

              inherit (pkgs') writeScriptBin stdenv;

              duneScript =
                writeScriptBin "dune" ''
                  #!${stdenv.shell}
                  "$DUNE_SOURCE_ROOT"/_boot/dune.exe $@
                '';
            in

            pkgs'.mkShell {
              shellHook = ''
                export DUNE_SOURCE_ROOT=$PWD
              '';
              inherit meta;
              nativeBuildInputs = (testNativeBuildInputs pkgs')
                ++ docInputs
                ++ [ duneScript ];
              inputsFrom = [ pkgs'.ocamlPackages.dune_3 ];
              buildInputs = testBuildInputs ++ (with pkgs'.ocamlPackages; [
                ocaml-lsp
                merlin
                ocaml-index
                ppx_expect
                spawn
                ctypes
                integers
                mdx
                cinaps
                menhir
                odoc
                lwt
                patdiff
              ] ++ (extraBuildInputs pkgs'));
            };
        in
        {
          doc =
            pkgs.mkShell {
              buildInputs = docInputs;
              meta.description = ''
                Provides a shell environment suitable for building the Dune
                documentation website (e.g. `make doc`).
              '';
            };

          fmt =
            pkgs.mkShell {
              nativeBuildInputs = [ ocamlformat ];
              inputsFrom = [ pkgs.dune_3 ];
              meta.description = ''
                Provides a shell environment suitable for formatting the Dune
                codebase source code (e.g. with `make fmt`).
              '';
            };

          slim = makeDuneDevShell {
            meta.description = ''
              Provides a minimal shell environment built purely from nixpkgs
              that can run the testsuite (except the coq / melange tests).
            '';
          };
          slim-melange = makeDuneDevShell {
            extraBuildInputs = pkgs: [
              pkgs.ocamlPackages.melange
            ];
            meta.description = ''
              Provides a minimal shell environment built purely from nixpkgs
              that can run the testsuite (except the coq tests).
            '';
          };
          slim-opam = with pkgs; mkShell {
            nativeBuildInputs = lib.remove pkgs.ocamlformat (testNativeBuildInputs pkgs);
            buildInputs = lib.optionals stdenv.isDarwin [
              darwin.apple_sdk.frameworks.CoreServices
            ];
            meta.description = ''
              provides a shell with just `opam` and minimal (external)
              dependencies to run the testsuite.";
            '';
          };

          coq =
            pkgs.mkShell {
              nativeBuildInputs = (testNativeBuildInputs pkgs);
              # Coq requires OCaml 4.x
              inputsFrom = [ pkgs.ocaml-ng.ocamlPackages_4_14.dune_3 ];
              buildInputs = with pkgs; [
                coq_8_16_native
                coq_8_16_native.ocamlPackages.findlib
              ];
              meta.description = ''
                Provides a minimal shell environment built purely from nixpkgs
                that can build Dune and the Coq testsuite.
              '';
            };
          microbench = makeDuneDevShell {
            extraBuildInputs = pkgs: [
              pkgs.ocamlPackages.core_bench
            ];
            meta.description = ''
              Provides a minimal shell environment that can build the
              microbenchmarks.
            '';
          };

          scope = makeDuneDevShell {
            duneFromScope = true;
            meta.description = ''
              Provides a minimal shell environment built purely from nixpkgs
              that replaces the Dune executable in the `ocamlPackages` scope by
              the Dune binary built by from the repo.
            '';
          };
          default =
            makeDuneDevShell {
              extraBuildInputs = pkgs: (with pkgs; [
                # dev tools
                ccls
                # test dependencies
                git
                which
                curl
                procps
              ]) ++ (with pkgs.ocamlPackages; [
                pkgs.ocamlPackages.ocaml-lsp
                pkgs.ocamlPackages.melange
                js_of_ocaml-compiler
                js_of_ocaml
                utop
                core_bench
              ]);
              meta.description = ''
                Provides a shell environment where `dune` is provided and built
                using the source code in this repo.
              '';
            };
        };
    });
}
