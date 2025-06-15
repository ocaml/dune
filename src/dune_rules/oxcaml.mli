open Import

(* [expand project macro ocaml] generates a list of values extracted from the
   version of OCaml. In this case, it will fail if OxCaml is not explicitely
   setup first. *)
val expand
  :  Dune_project.t
  -> Pform.Macro_invocation.t
  -> Ocaml_toolchain.t
  -> Value.t list
