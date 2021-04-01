Test the behaviour of Dune querying git when the repo has no commit.

Create a repository with no HEAD commit:

  $ git init --quiet

... and an executable that links `dune-build-info':

  $ echo "(lang dune 2.0)" > dune-project
  $ touch main.ml
  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries dune-build-info)
  >  (promote (until-clean)))
  > EOF

At the moment Dune fails, which is bad:

  $ dune exec ./main.exe 2>&1 | sed 's/.*\/git/{{ git }}/; s/> .*.output/> {{ output_file }}/g' 
           git (internal) (exit 128)
  {{ git }} describe --always --dirty > {{ output_file }}
  fatal: bad revision 'HEAD'

