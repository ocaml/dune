Test the 'dune promotion show' command

=== WHAT THIS TEST VERIFIES ===
This test ensures that 'dune promotion show' correctly displays the contents
of corrected files that are ready for promotion, without modifying the source
files. The command allows users to preview what changes would be applied before
running 'dune promote'.

Key properties tested:
1. Shows corrected content for files with failed diff rules
2. Is read-only (never modifies source files)
3. Handles single file, multiple files, and all files (no args)
4. Proper warning handling for non-promotable files
5. Files with 'diff' (not 'diff?') are promotable

=== WHY THIS MATTERS ===
The 'show' command is critical for user workflow: it lets developers review
auto-generated corrections before accepting them. This is especially important
for generated code, test expectations, or formatted output where users need to
verify the changes make sense before committing them.

=== HOW TO VERIFY CORRECTNESS ===
1. Before 'runtest': show should warn (no corrections available)
2. After 'runtest': show should display corrected content from _build
3. After 'show': source files should remain unchanged (read-only guarantee)
4. Output includes trailing newline after each file's content
5. Only 'diff' rules create promotions; 'diff?' rules may not always generate them

Setup a basic dune project with promotion-capable rules

We create different promotion scenarios to test various behaviors:
- a.expected: uses 'diff' (creates promotion when files differ)
- b.expected: uses 'diff?' in progn (creates promotion when files differ)
- c.expected: uses 'diff?' in separate action (may not create promotion)

  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (alias runtest)
  >  (action
  >   (diff a.expected a.actual)))
  > 
  > (rule
  >  (with-stdout-to a.actual
  >   (echo "A actual\n")))
  > 
  > (rule
  >  (alias runtest)
  >  (action
  >   (progn
  >    (with-stdout-to b.actual
  >     (echo "B actual\n"))
  >    (diff? b.expected b.actual))))
  > 
  > (rule
  >  (with-stdout-to c.actual
  >   (echo "C actual\n")))
  > 
  > (rule
  >  (alias runtest)
  >  (action
  >   (diff? c.expected c.actual)))
  > EOF

  $ echo 'A expected' > a.expected
  $ echo 'B expected' > b.expected
  $ echo 'C expected' > c.expected

=== TEST: Before tests run, no promotions available ===
When no tests have run yet, there are no corrected files in _build,
so 'show' should return a warning (not an error, as this is informational).

  $ dune promotion show a.expected 2>&1
  Warning: Nothing to promote for a.expected.

=== TEST: Generate corrections by running tests ===
Running tests will create .actual files in _build that differ from
the .expected files. Note that only 'diff' and some 'diff?' rules
actually create promotable files.

  $ dune runtest 2>&1
  File "a.expected", line 1, characters 0-0:
  Error: Files _build/default/a.expected and _build/default/a.actual differ.
  File "b.expected", line 1, characters 0-0:
  Error: Files _build/default/b.expected and _build/default/b.actual differ.
  [1]

=== TEST: Show corrected contents of a single file ===
After tests run, 'show' should display the contents of the corrected
file from _build without modifying the source file.
Note: Output includes a trailing newline after the content.

  $ dune promotion show a.expected
  A actual
  

=== TEST: Show another single file ===
Verify 'show' works consistently for different files.

  $ dune promotion show b.expected
  B actual
  

=== TEST: Show multiple files at once ===
The command should accept multiple file arguments and display their
corrected contents in the order specified.
Each file's content is followed by a newline.

  $ dune promotion show a.expected b.expected
  A actual
  
  B actual
  

=== TEST: Mixed valid and invalid arguments ===
When some arguments are promotable and others are not, the command should:
- Show warnings for invalid files (appears before content)
- Continue processing valid files
- Display content for all valid files

This is important for scripting and batch operations where partial
success is better than complete failure.

  $ touch nothing-to-promote.txt
  $ dune promotion show a.expected nothing-to-promote.txt b.expected 2>&1
  Warning: Nothing to promote for nothing-to-promote.txt.
  A actual
  
  B actual
  

=== TEST: Non-existent file handling ===
When a specified file doesn't exist at all (not just non-promotable),
the command fails with a usage error and exit code 1.

This is different from "nothing to promote" - the file must exist
in the filesystem to be checked for promotions.

  $ dune promotion show does-not-exist.ml 2>&1
  dune: FILE… arguments: no 'does-not-exist.ml' file or directory
  Usage: dune promotion show [OPTION]… [FILE]…
  Try 'dune promotion show --help' or 'dune --help' for more information.
  [1]

=== TEST: Show all corrected files (no arguments) ===
When called without arguments, 'show' should display all files that
have corrections available. The order may vary.

  $ dune promotion show
  B actual
  
  A actual
  

=== TEST: Verify read-only behavior ===
Critical property: 'show' must never modify source files.
After all the 'show' operations above, source files should still
contain their original "expected" content, not the "actual" content.

  $ cat a.expected
  A expected
  $ cat b.expected
  B expected
  $ cat c.expected
  C expected

=== TEST: Files with diff? may not always create promotions ===
The c.expected file uses 'diff?' in a separate action, which doesn't
always create a promotion entry. This is expected behavior.

  $ dune promotion show c.expected > c.shown
  Warning: Nothing to promote for c.expected.
  $ dune promote c.expected 2>&1
  Warning: Nothing to promote for c.expected.

Since there was nothing to promote, c.expected remains unchanged
and the diff shows they're different (original vs unchanged).

  $ diff c.shown c.expected
  0a1
  > C expected
  [1]

=== TEST: After showing, files remain available ===
The 'show' command is read-only, so files remain available for
promotion after being shown.

  $ dune promotion show c.expected 2>&1
  Warning: Nothing to promote for c.expected.

=== TEST: Other files still available ===
Files that haven't been promoted remain available.

  $ dune promotion show a.expected
  A actual
  

=== EDGE CASE: Empty corrected file ===
Empty files are a valid edge case - a diff might result in deleting
all content. However, 'diff?' may not create a promotion for such files.

This tests robustness against zero-length file content.

  $ cat > dune << EOF
  > (rule
  >  (alias runtest)
  >  (action
  >   (diff? empty.expected empty.actual)))
  > 
  > (rule
  >  (with-stdout-to empty.actual
  >   (progn)))
  > EOF

  $ echo 'not empty' > empty.expected
  $ dune runtest 2>&1

In this case, 'diff?' doesn't create a promotion entry for the empty file.

  $ dune promotion show empty.expected
  Warning: Nothing to promote for empty.expected.

=== EDGE CASE: Multiline content ===
Real-world promoted files often contain multiple lines. However, we need
to verify whether 'diff?' creates promotions for such files.

  $ cat > dune << EOF
  > (rule
  >  (alias runtest)
  >  (action
  >   (diff? multi.expected multi.actual)))
  > 
  > (rule
  >  (with-stdout-to multi.actual
  >   (progn
  >    (echo "line 1\n")
  >    (echo "line 2\n")
  >    (echo "line 3\n"))))
  > EOF

  $ echo 'old content' > multi.expected
  $ dune runtest 2>&1

This 'diff?' rule also doesn't create a promotion entry.

  $ dune promotion show multi.expected
  Warning: Nothing to promote for multi.expected.

=== TEST: Multiple non-promotable files ===
When multiple files have no promotions available, each gets a warning.

  $ dune promotion show multi.expected empty.expected
  Warning: Nothing to promote for empty.expected.
  Warning: Nothing to promote for multi.expected.
