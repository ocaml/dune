#!/bin/bash

version=59a318563d745e63fc69f3d58a8a8ff302e91c4b

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

pkg=patch

rm -rf $pkg
mkdir -p $pkg/src

(
	cd $TMP
	git clone https://github.com/hannesm/$pkg.git
	cd $pkg
	git checkout $version
)

src=$TMP/$pkg

cp -v $src/src/patch.{ml,mli} $pkg/src/
cp -v $src/src/lib.{ml,mli} $pkg/src/
cp -v $src/src/fname.{ml,mli} $pkg/src/
cp -v $src/src/rope.{ml,mli} $pkg/src/
cp -v $src/LICENSE.md $pkg/

# When ---/+++ headers indicate an Edit but git metadata says "new file mode"
# or "deleted file mode", let the ---/+++ headers take precedence instead of
# emitting a spurious Git_ext Create_only/Delete_only with empty hunks.
cat > $TMP/spurious-file-mode.patch << "EOF"
--- a/src/patch.ml
+++ b/src/patch.ml
@@ -430,7 +430,8 @@
           when String.equal f f' -> Some op, xs
         | Some (a, b, Rename_only (_, _)), (Edit (a', b') as op)
           when String.equal a a' && String.equal b b' -> Some op, xs
-        | Some (_, _, (Rename_only _ | Delete_only | Create_only) as git_op), _
+        | Some (_, _, (Create_only | Delete_only)), op -> Some op, xs
+        | Some (_, _, Rename_only _ as git_op), _
           -> Some (Git_ext git_op), x :: y :: xs
         end
     | x::y::_xs when Lib.String.is_prefix ~prefix:"*** " x && Lib.String.is_prefix ~prefix:"--- " y ->
EOF
patch -p1 -d $pkg < $TMP/spurious-file-mode.patch

git add -A .
