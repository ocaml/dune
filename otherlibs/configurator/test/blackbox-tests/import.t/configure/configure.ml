(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2022 Liang Wang <liang@ocaml.xyz>
 *)

module Configurator = Configurator.V1

let header = {|
    #define TEST "test"
|}

let default_cflags c =
  let test =
    let headerfile =
      let file, fd =
        Filename.open_temp_file ~mode:[ Open_wronly ] "discover" "test.h"
      in
      output_string fd header;
      close_out fd;
      file
    in
    let platform =
      assert (Sys.file_exists headerfile);
      Configurator.C_define.import c ~includes:[ headerfile ] [ ("TEST", String) ]
    in
    match List.map snd platform with
    | [ String "test" ] -> `test
    | _ -> `unknown
  in
  match test with `test -> [] | _ -> assert false

let () =
  let flags_file = ref "" in
  let args = ["-target", Arg.Set_string flags_file , "flags file"] in
  Configurator.main ~args ~name:"test" (fun c ->
      let libs = [] in
      let cflags = default_cflags c in
      let conf : Configurator.Pkg_config.package_conf = { cflags; libs } in
      Configurator.Flags.write_sexp !flags_file conf.cflags)
