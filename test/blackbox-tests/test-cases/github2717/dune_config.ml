module C = Configurator.V1

let gen_pkg p ~package ~version =
  let file kind = kind ^ "-" ^ package ^ ".sexp" in
  let c_g =
    let expr = match version with
      | None -> package
      | Some v -> Printf.sprintf "%s >= %s" package v
    in
    Result.get_ok (C.Pkg_config.query_expr_err p ~package ~expr)
  in
  C.Flags.write_sexp (file "cflag") c_g.C.Pkg_config.cflags;
  C.Flags.write_sexp (file "clink") c_g.C.Pkg_config.libs

let pkg = ref ""
let version = ref None

let main t =
  let p = Option.get C.Pkg_config.(get t) in
  gen_pkg p ~package:!pkg ~version:!version

let _ =
  let args = [ "-pkg", Arg.String (fun s -> pkg := s), "package"
             ; "-version", Arg.String (fun v -> version := Some v), "version" ] in
  C.main ~args ~name:"lablgtk3" main
