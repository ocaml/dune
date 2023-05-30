Setting environment variables in actions

  $ mkdir dune.lock
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF
  $ cat >dune.lock/test <<'EOF'
  > (build
  >  (withenv
  >   ((= FOO myfoo)
  >    (= XYZ 000)
  >    (+= XYZ 111)
  >    (= BAR xxx)
  >    (+= BAR yyy)
  >    (:= BAR "")
  >    (+= BAR ""))
  >   (system "echo XYZ=$XYZ; echo FOO=$FOO; echo BAR=$BAR")))
  > EOF
  $ dune build .pkg/test/target/
  XYZ=111:000
  FOO=myfoo
  BAR=yyy:xxx
