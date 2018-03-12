
val apply_for_compile_info
  :  Coverage0.Context.t
  -> lib:Jbuild.Library.t
  -> Jbuild.Library.t

val apply_instrumented
  :  Coverage0.Context.t
  -> lib:Jbuild.Library.t
  -> modules:Module.t Module.Name.Map.t
  -> Jbuild.Library.t
