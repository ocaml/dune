open Stdune

let span_is_nonnegative span = Time.Span.compare span Time.Span.zero <> Lt
let is_monotonic ~before ~after = Time.Span.compare before after <> Gt

let usage_is_sane
      { Proc.Resource_usage.user_cpu_time
      ; system_cpu_time
      ; maxrss
      ; minflt
      ; majflt
      ; inblock
      ; oublock
      ; nvcsw
      ; nivcsw
      }
  =
  span_is_nonnegative user_cpu_time
  && span_is_nonnegative system_cpu_time
  && maxrss >= 0
  && minflt >= 0
  && majflt >= 0
  && inblock >= 0
  && oublock >= 0
  && nvcsw >= 0
  && nivcsw >= 0
;;

let usage_does_not_go_backwards
      { Proc.Resource_usage.user_cpu_time = before_user_cpu_time
      ; system_cpu_time = before_system_cpu_time
      ; maxrss = before_maxrss
      ; minflt = before_minflt
      ; majflt = before_majflt
      ; inblock = before_inblock
      ; oublock = before_oublock
      ; nvcsw = before_nvcsw
      ; nivcsw = before_nivcsw
      }
      { Proc.Resource_usage.user_cpu_time = after_user_cpu_time
      ; system_cpu_time = after_system_cpu_time
      ; maxrss = after_maxrss
      ; minflt = after_minflt
      ; majflt = after_majflt
      ; inblock = after_inblock
      ; oublock = after_oublock
      ; nvcsw = after_nvcsw
      ; nivcsw = after_nivcsw
      }
  =
  is_monotonic ~before:before_user_cpu_time ~after:after_user_cpu_time
  && is_monotonic ~before:before_system_cpu_time ~after:after_system_cpu_time
  && before_maxrss <= after_maxrss
  && before_minflt <= after_minflt
  && before_majflt <= after_majflt
  && before_inblock <= after_inblock
  && before_oublock <= after_oublock
  && before_nvcsw <= after_nvcsw
  && before_nivcsw <= after_nivcsw
;;

let print_bool name value = Printf.printf "%s: %b\n" name value

let%expect_test "Proc.Resource_usage.get_self returns sane data" =
  let before = Proc.Resource_usage.get_self () in
  let busy = ref 0 in
  for i = 1 to 100_000 do
    busy := !busy + i
  done;
  ignore !busy;
  let after = Proc.Resource_usage.get_self () in
  let available_matches_platform =
    Bool.equal (Option.is_some before) (not Sys.win32)
    && Bool.equal (Option.is_some after) (not Sys.win32)
  in
  let values_are_sane =
    match before, after with
    | Some before, Some after -> usage_is_sane before && usage_is_sane after
    | None, None -> true
    | _ -> false
  in
  let values_do_not_go_backwards =
    match before, after with
    | Some before, Some after -> usage_does_not_go_backwards before after
    | None, None -> true
    | _ -> false
  in
  print_bool "available_matches_platform" available_matches_platform;
  print_bool "values_are_sane" values_are_sane;
  print_bool "values_do_not_go_backwards" values_do_not_go_backwards;
  [%expect
    {|
    available_matches_platform: true
    values_are_sane: true
    values_do_not_go_backwards: true
  |}]
;;
