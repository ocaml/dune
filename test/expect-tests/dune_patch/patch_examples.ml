open Stdune

(* Patch string examples for testing *)

(* Basic example adding and removing a line. *)
let basic =
  {|
diff --git a/foo.ml b/foo.ml
index b69a69a5a..ea988f6bd 100644
--- a/foo.ml
+++ b/foo.ml
@@ -1,1 +1,1 @@
-This is wrong
+This is right
|}
;;

(* Example adding and removing a line in a file in a subdirectory. *)
let subdirectory =
  {|
diff --git a/dir/foo.ml b/dir/foo.ml
index b69a69a5a..ea988f6bd 100644
--- a/dir/foo.ml
+++ b/dir/foo.ml
@@ -1,1 +1,1 @@
-This is wrong
+This is right
|}
;;

(* Previous two example combined into a single patch. *)
let combined = String.concat ~sep:"\n" [ basic; subdirectory ]

(* Example adding a new file. *)
let new_file =
  {|
diff --git a/foo.ml b/foo.ml
new file mode 100644
index 000000000..ea988f6bd
--- /dev/null
+++ b/foo.ml
@@ -0,0 +1,2 @@
+This is right
+
|}
;;

(* Example deleting an existing file. *)
let delete_file =
  {|
diff --git a/foo.ml b/foo.ml
deleted file mode 100644
index ea988f6bd..000000000
--- a/foo.ml
+++ /dev/null
@@ -1,1 +0,0 @@
-This is wrong
|}
;;

(* Use GNU diff 'unified' format instead of 'git diff' *)
let unified =
  {|
diff -u a/foo.ml b/foo.ml
--- a/foo.ml	2024-08-29 17:37:53.114980665 +0200
+++ b/foo.ml	2024-08-29 17:38:00.243088256 +0200
@@ -1 +1 @@
-This is wrong
+This is right
|}
;;

let no_prefix =
  {|
--- foo.ml	2024-08-29 17:37:53.114980665 +0200
+++ foo.ml	2024-08-29 17:38:00.243088256 +0200
@@ -1 +1 @@
-This is wrong
+This is right
|}
;;

let random_prefix =
  {|
diff -u bar/foo.ml baz/foo.ml
--- bar/foo.ml	2024-08-29 17:37:53.114980665 +0200
+++ baz/foo.ml	2024-08-29 17:38:00.243088256 +0200
@@ -1 +1 @@
-This is wrong
+This is right
|}
;;

(* The file is called "foo bar" *)
let spaces =
  {|
diff --git a/foo bar b/foo bar
index ef00db3..88adca3 100644
--- a/foo bar
+++ b/foo bar
@@ -1 +1 @@
-This is wrong
+This is right
|}
;;

(* The file is called "foo bar" but in unified diff its quoted *)
let unified_spaces =
  {|
--- "a/foo bar"	2024-09-04 10:56:24.139293679 +0200
+++ "b/foo bar"	2024-09-04 10:56:12.519195763 +0200
@@ -1 +1 @@
-This is wrong
+This is right
|}
;;

let hello_world =
  {|
diff --git a/foo.ml b/foo.ml
new file mode 100644
index 0000000..557db03
--- /dev/null
+++ b/foo.ml
@@ -0,0 +1 @@
+Hello World
|}
;;

let rename_patch =
  {|
diff --git a/old.ml b/new.ml
similarity index 100%
rename from old.ml
rename to new.ml
|}
;;

let git_ext_delete_only =
  {|
diff --git a/foo.ml b/foo.ml
deleted file mode 100644
index 557db03..0000000
--- a/foo.ml
+++ /dev/null
@@ -1 +0,0 @@
-Hello World
|}
;;

let git_ext_create_only =
  {|
diff --git a/foo.ml b/foo.ml
new file mode 100644
index 0000000..557db03
--- /dev/null
+++ b/foo.ml
@@ -0,0 +1 @@
+Hello World
|}
;;

let edit_with_rename =
  {|
diff --git a/source.ml b/target.ml
index b69a69a5a..ea988f6bd 100644
--- a/source.ml
+++ b/target.ml
@@ -1,1 +1,1 @@
-This is wrong
+This is right
|}
;;
