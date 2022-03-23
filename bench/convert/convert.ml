open Base
open Stdio

type bench_result =
  { full_benchmark_name : string
  ; time_per_run_nanos : float
  ; minor_words_per_run : float
  ; major_words_per_run : float
  ; promoted_words_per_run : float
  }
[@@deriving of_sexp] [@@sexp.allow_extra_fields]

let convert
    { full_benchmark_name
    ; time_per_run_nanos
    ; minor_words_per_run
    ; major_words_per_run
    ; promoted_words_per_run
    } =
  let metrics =
    let open Bench_format.Metric in
    [ { name = "time"; value = F time_per_run_nanos; units = "nanosecs" }
    ; { name = "minor_words"; value = F minor_words_per_run; units = "words" }
    ; { name = "major_words"; value = F major_words_per_run; units = "words" }
    ; { name = "promoted_words"
      ; value = F promoted_words_per_run
      ; units = "words"
      }
    ]
  in
  { Bench_format.Bench_result.name = full_benchmark_name; metrics }

let main input_path =
  let contents = In_channel.read_all input_path in
  let sexp = Parsexp.Single.parse_string_exn contents in
  let result_list = [%of_sexp: bench_result list] sexp in
  let json =
    Bench_format.yojson_of_t
      { version = V2; results = List.map ~f:convert result_list }
  in
  Yojson.Safe.pretty_to_channel Stdlib.stdout json

let () =
  match Sys.get_argv () with
  | [| _; input_path |] -> main input_path
  | _ -> assert false
