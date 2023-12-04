#!/usr/bin/env bash

version=7c05922290adb214f834d9e0f1efe7028c4ebb85

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf opam
mkdir -p opam/src

(
    cd $TMP
    git clone https://github.com/ocaml-dune/opam.git
    cd opam
    git checkout $version
)

SRC=$TMP/opam

cat > $TMP/local-opam-changes.diff << "EOF"
diff --git a/vendor/opam/src/core/opamCoreConfigDeveloper.ml b/vendor/opam/src/core/opamCoreConfigDeveloper.ml
new file mode 100644
index 000000000..cf62a7fd2
--- /dev/null
+++ b/vendor/opam/src/core/opamCoreConfigDeveloper.ml
@@ -0,0 +1 @@
+let value = ""
diff --git a/vendor/opam/src/core/opamProcess.ml b/vendor/opam/src/core/opamProcess.ml
index 764ff18fd..ef8734625 100644
--- a/vendor/opam/src/core/opamProcess.ml
+++ b/vendor/opam/src/core/opamProcess.ml
@@ -9,6 +9,8 @@
 (*                                                                        *)
 (**************************************************************************)
 
+module Re = Dune_re
+
 let log ?level fmt =
   OpamConsole.log "PROC" ?level fmt
 
diff --git a/vendor/opam/src/core/opamStd.ml b/vendor/opam/src/core/opamStd.ml
index e04c84379..454702714 100644
--- a/vendor/opam/src/core/opamStd.ml
+++ b/vendor/opam/src/core/opamStd.ml
@@ -9,6 +9,8 @@
 (*                                                                        *)
 (**************************************************************************)
 
+module Re = Dune_re
+
 module type SET = sig
   include Set.S
   val map: (elt -> elt) -> t -> t
diff --git a/vendor/opam/src/core/opamStd.mli b/vendor/opam/src/core/opamStd.mli
index 9133f9e16..5fd460f06 100644
--- a/vendor/opam/src/core/opamStd.mli
+++ b/vendor/opam/src/core/opamStd.mli
@@ -278,7 +278,7 @@ module String : sig
   val for_all: (char -> bool) -> string -> bool
   val contains_char: string -> char -> bool
   val contains: sub:string -> string -> bool
-  val exact_match: Re.re -> string -> bool
+  val exact_match: Dune_re.re -> string -> bool
   val find_from: (char -> bool) -> string -> int -> int
 
   (** Like [String.compare], but with lowercase/uppercase variants ordered next
diff --git a/vendor/opam/src/core/opamUrl.ml b/vendor/opam/src/core/opamUrl.ml
index 16baf4c75..8234b3419 100644
--- a/vendor/opam/src/core/opamUrl.ml
+++ b/vendor/opam/src/core/opamUrl.ml
@@ -11,6 +11,8 @@
 
 open OpamStd.Op
 
+module Re = Dune_re
+
 type version_control = [ `git | `darcs | `hg ]
 
 type backend = [ `http | `rsync | version_control ]
diff --git a/vendor/opam/src/core/opamVersionInfo.ml b/vendor/opam/src/core/opamVersionInfo.ml
new file mode 100644
index 000000000..6c0168d9b
--- /dev/null
+++ b/vendor/opam/src/core/opamVersionInfo.ml
@@ -0,0 +1 @@
+let version = "2.2.0~alpha-vendored"
diff --git a/vendor/opam/src/format/opamFile.ml b/vendor/opam/src/format/opamFile.ml
index 03611c8e8..689cac471 100644
--- a/vendor/opam/src/format/opamFile.ml
+++ b/vendor/opam/src/format/opamFile.ml
@@ -18,6 +18,7 @@
       a string list list. These are mostly used internally
     - files using the "opam syntax" and lexer, parsed using OpamFormat.Pp.V
 *)
+module Re = Dune_re
 
 open OpamParserTypes.FullPos
 open OpamTypes
diff --git a/vendor/opam/src/format/opamFilter.ml b/vendor/opam/src/format/opamFilter.ml
index af7374e4b..d60abc796 100644
--- a/vendor/opam/src/format/opamFilter.ml
+++ b/vendor/opam/src/format/opamFilter.ml
@@ -8,6 +8,7 @@
 (*  exception on linking described in the file LICENSE.                   *)
 (*                                                                        *)
 (**************************************************************************)
+module Re = Dune_re
 
 open OpamTypes
 open OpamTypesBase
diff --git a/vendor/opam/src/format/opamFormula.ml b/vendor/opam/src/format/opamFormula.ml
index 6931055e4..277b17963 100644
--- a/vendor/opam/src/format/opamFormula.ml
+++ b/vendor/opam/src/format/opamFormula.ml
@@ -9,6 +9,8 @@
 (*                                                                        *)
 (**************************************************************************)
 
+module Re = Dune_re
+
 type relop = [`Eq|`Neq|`Geq|`Gt|`Leq|`Lt]
 
 let neg_relop = function
diff --git a/vendor/opam/src/format/opamSwitch.ml b/vendor/opam/src/format/opamSwitch.ml
index c6704f911..6dbd58e96 100644
--- a/vendor/opam/src/format/opamSwitch.ml
+++ b/vendor/opam/src/format/opamSwitch.ml
@@ -9,6 +9,8 @@
 (*                                                                        *)
 (**************************************************************************)
 
+module Re = Dune_re
+
 include OpamStd.AbstractString
 
 let unset = of_string "#unset#"
diff --git a/vendor/opam/src/state/opamEnv.ml b/vendor/opam/src/state/opamEnv.ml
index 8913edda0..044835ad0 100644
--- a/vendor/opam/src/state/opamEnv.ml
+++ b/vendor/opam/src/state/opamEnv.ml
@@ -15,6 +15,7 @@ open OpamStateTypes
 open OpamTypesBase
 open OpamStd.Op
 open OpamFilename.Op
+module Re = Dune_re
 
 let log fmt = OpamConsole.log "ENV" fmt
 let slog = OpamConsole.slog
diff --git a/vendor/opam/src/state/opamFileTools.ml b/vendor/opam/src/state/opamFileTools.ml
index acba124a7..0e6f051ac 100644
--- a/vendor/opam/src/state/opamFileTools.ml
+++ b/vendor/opam/src/state/opamFileTools.ml
@@ -12,6 +12,7 @@
 open OpamParserTypes.FullPos
 open OpamTypes
 open OpamTypesBase
+module Re = Dune_re
 
 let log ?level fmt = OpamConsole.log "opam-file" ?level fmt
 
diff --git a/vendor/opam/src/state/opamSysInteract.ml b/vendor/opam/src/state/opamSysInteract.ml
index 2857654a9..53f11d870 100644
--- a/vendor/opam/src/state/opamSysInteract.ml
+++ b/vendor/opam/src/state/opamSysInteract.ml
@@ -8,6 +8,8 @@
 (*                                                                        *)
 (**************************************************************************)
 
+module Re = Dune_re
+
 let log fmt = OpamConsole.log "XSYS" fmt
 
 (* Run commands *)

diff --git a/vendor/opam/src/format/opamFilter.mli b/vendor/opam/src/format/opamFilter.mli
index a2f01d179..1787b24b1 100644
--- a/vendor/opam/src/format/opamFilter.mli
+++ b/vendor/opam/src/format/opamFilter.mli
@@ -8,6 +8,7 @@
 (*  exception on linking described in the file LICENSE.                   *)
 (*                                                                        *)
 (**************************************************************************)
+module Re = Dune_re

 (** Formulas on variables, as used in opam files build scripts

EOF

for subpackage in core repository format state
do
    PKG=opam/src/$subpackage/
    mkdir -p $PKG
    set +e
    cp -v $SRC/src/$subpackage/*.{ml,mli,mll,mly,c} $PKG
    set -e
    git checkout $PKG/dune
done

git apply $TMP/local-opam-changes.diff
git add -A opam/
