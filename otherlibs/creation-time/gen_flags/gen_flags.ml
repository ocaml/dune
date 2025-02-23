let statx =
  {|
#define _GNU_SOURCE
#include <fcntl.h>
#include <sys/stat.h>

int main(void) {
    struct statx stx = { 0, };
    int rc = statx(AT_FDCWD, "", AT_EMPTY_PATH, STATX_BTIME, &stx);
    return rc;
}
|}
;;

let checks = [ "HAVE_STATX", statx ]

let run_check cc check =
  let temp = Filename.temp_file "dune-gen-flags-" ".c" in
  let oc = open_out temp in
  output_string oc check;
  close_out oc;
  let cmd = cc.(0) in
  let stdout, stdin, stderr =
    Unix.open_process_args_full cmd (Array.append cc [| temp |]) (Unix.environment ())
  in
  let _ : string = In_channel.input_all stdout in
  let _ : string = In_channel.input_all stderr in
  let status = Unix.close_process_full (stdout, stdin, stderr) in
  match status with
  | WEXITED 0 -> true
  | WEXITED _ | WSIGNALED _ | WSTOPPED _ -> false
;;

let () =
  let cc = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
  let haves =
    List.filter_map
      (fun (var, check) ->
        match run_check cc check with
        | true -> Some (Printf.sprintf "-D%s " var)
        | false -> None)
      checks
  in
  print_string "(";
  List.iter print_string haves;
  print_endline ")"
;;
