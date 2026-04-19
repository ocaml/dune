open Import

(* This module is separate from [Artifacts] to avoid cycles *)

val get : Context.t -> Artifacts.t Memo.t

val resolve_package_install_file
  :  Context_name.t
  -> pkg:Package.Name.t
  -> section:Section.t
  -> file:string
  -> Path.Build.t option Memo.t
