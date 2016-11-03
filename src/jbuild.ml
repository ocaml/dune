module J = Jbuild_interpret

let () = Future.Scheduler.go (Rule.do_build ["all"])
