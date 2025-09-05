open! Stdune

let cwd () = Path.external_ (Path.External.cwd ())

let chdir dir = Unix.chdir (Path.to_absolute_filename dir)

let prefix = "test.exe."

let suffix = ".t"

let file name c =
  let path = Path.relative (cwd ()) name in
  printfn "-> creating %s" name;
  Io.write_file path c

let descr s f =
  printfn "# %s" s;
  let res = f () in
  print_endline "";
  res

let sh =
  let path = Env_path.path Env.initial in
  Option.value_exn (Bin.which ~path "sh")

let in_dir name f =
  let cwd = cwd () in
  let dir = Path.relative cwd name in
  Path.mkdir_p dir;
  chdir dir;
  Exn.protect ~f ~finally:(fun () -> chdir cwd)

let cmd_res s =
  let script = Temp.create File ~prefix ~suffix in
  Io.write_file script s;
  print_endline ("% " ^ s);
  let pid =
    let prog = Path.to_absolute_filename sh in
    Spawn.spawn ~stderr:Unix.stdout ~stdout:Unix.stdout ~prog
      ~argv:[ prog; Path.to_absolute_filename script ]
      ()
    |> Pid.of_int
  in
  let pid', code = Unix.wait () in
  assert (Pid.equal pid (Pid.of_int pid'));
  match code with
  | WEXITED 0 -> Ok ()
  | WEXITED n ->
    printfn "[%d]" n;
    Error ()
  | _ -> Error ()

let cmd s =
  match cmd_res s with
  | Error () -> ()
  | Ok () -> ()

module Spec = struct
  type t =
    { mli_only : bool
    ; wrapped : bool
    ; stubs : bool
    }

  let name { mli_only; wrapped; stubs } =
    sprintf "%s_%s_%s"
      (if mli_only then
        "mli_only"
      else
        "no_mli")
      (if wrapped then
        "wrapped"
      else
        "unwrapped")
      (if stubs then
        "stubs"
      else
        "no_stubs")

  let all =
    let for_all f = List.concat_map [ true; false ] ~f in
    for_all (fun mli_only ->
        for_all (fun wrapped ->
            for_all (fun stubs -> [ { mli_only; wrapped; stubs } ])))

  type res =
    { internal : bool
    ; external_ : bool
    }

  let run_body t () =
    let res = ref { internal = false; external_ = false } in
    let cmd_report k s =
      let new_res = Result.is_ok (cmd_res s) in
      res :=
        match k with
        | `Internal -> { !res with internal = new_res }
        | `External -> { !res with external_ = new_res }
    in
    file "dune-project" {|(lang dune 2.8)
(package
 (name foo))
|};
    descr "build the library and see if .a is present" (fun () ->
        in_dir "lib" (fun () ->
            let stubs =
              if t.stubs then
                "(foreign_stubs (language c) (names stub))"
              else
                ""
            in
            file "dune"
              (Printf.sprintf
                 {|
(library
 (public_name foo)
 (wrapped %b)
 %s
 (modules ()))
|}
                 t.wrapped stubs);
            if t.stubs then file "stub.c" "void foo() {}";
            if t.mli_only then file "foo.mli" "type x = unit";
            cmd "cat dune");
        cmd "dune build --root . @install";
        cmd "ls _build/install/default/lib/foo/*.a");

    descr "create a dummy executable to test" (fun () ->
        in_dir "exe" (fun () ->
            file "dune"
              {|
(executable
 (name b)
 (link_flags -linkall)
 (libraries foo))
|};
            file "b.ml" "print_endline \"exe working\""));

    descr "make sure that this library is usable locally" (fun () ->
        cmd_report `Internal "dune exec ./exe/b.exe");

    descr "make sure that this library is usable externally" (fun () ->
        cmd "rm -rf lib";
        cmd_report `External
          "OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 \
           ./exe/b.exe");
    !res

  let run t =
    let name = name t in
    descr name (fun () -> in_dir name (run_body t))

  let report t { external_; internal } =
    let name = name t in
    let res x =
      if x then
        "pass"
      else
        "fail"
    in
    Printf.printf "%s - external - %s\n" name (res external_);
    Printf.printf "%s - internal - %s\n" name (res internal)
end

let () =
  Spec.all
  |> List.map ~f:(fun t -> (t, Spec.run t))
  |> List.iter ~f:(fun (t, res) -> Spec.report t res)
