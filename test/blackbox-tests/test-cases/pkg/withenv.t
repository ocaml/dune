Setting environment variables in actions

  $ make_lockdir
  $ make_lockpkg test <<'EOF'
  > (version 0.0.1)
  > (build
  >  (withenv
  >   ((= FOO myfoo)
  >    (= XYZ 000)
  >    (+= XYZ 111)
  >    (= BAR xxx)
  >    (+= BAR yyy)
  >    (:= BAR "")
  >    (+= BAR "")
  >    (:= BAZ baz)
  >    (=: QUX qux))
  >   (system "echo XYZ=$XYZ; echo FOO=$FOO; echo BAR=$BAR; echo BAZ=$BAZ; echo QUX=$QUX")))
  > EOF
  $ build_pkg test
  XYZ=111:000
  FOO=myfoo
  BAR=yyy:xxx
  BAZ=baz:
  QUX=:qux

The environment should also apply when the package's source is prepared
because of a lock-directory file overlay:

  $ mkdir overlay-source
  $ echo original > overlay-source/origin
  $ make_lockpkg overlay <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/overlay-source))
  > (build
  >  (withenv
  >   ((= FOO myfoo))
  >   (system "echo FOO=\$FOO; cat overlaid; echo changed > origin")))
  > EOF
  $ make_lockpkg_file overlay overlaid <<EOF
  > overlaid file
  > EOF
  $ build_pkg overlay
  FOO=myfoo
  overlaid file
  $ cat overlay-source/origin
  original

It should also apply when the source is prepared because of an extra source:

  $ mkdir extra-source
  $ echo extra > extra-file
  $ make_lockpkg extra <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/extra-source))
  > (extra_sources (extra (copy $PWD/extra-file)))
  > (build
  >  (withenv
  >   ((= FOO myfoo))
  >   (system "echo FOO=\$FOO; cat extra")))
  > EOF
  $ build_pkg extra
  FOO=myfoo
  extra

It should also apply to the rest of a recipe whose leading patch is inside
the environment's scope:

  $ mkdir patched-source
  $ make_lockpkg patched <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/patched-source))
  > (build
  >  (withenv
  >   ((= FOO myfoo))
  >   (progn
  >    (patch foo.patch)
  >    (system "echo FOO=\$FOO; cat foo.ml"))))
  > EOF
  $ make_lockpkg_file patched foo.patch <<EOF
  > diff --git a/foo.ml b/foo.ml
  > new file mode 100644
  > index 0000000..557db03
  > --- /dev/null
  > +++ b/foo.ml
  > @@ -0,0 +1 @@
  > +Hello World
  > EOF
  $ build_pkg patched
  FOO=myfoo
  Hello World
