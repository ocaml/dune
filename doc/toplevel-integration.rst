********************
Toplevel integration
********************

It's possible to load dune projects in any toplevel. This is achieved in two stages.

First, `dune toplevel-init-file` builds the project and produces a list of toplevel pragmas
(#directory and #load). Copying the output of this command to a toplevel lets you
interact with the project's modules.

Second, to enhance usability, dune also provides a toplevel script, which does the above
manual work for you. To use it, make sure to have `topfind` available in your toplevel by
invoking `#use "topfind";;`. Afterwards you can run `#use "dune";;` and your
modules should be available.