open Import

(* The opam [build:] shapes dune emits in its generated opam files.
   Exposes both a generator ([default_build_command_value], called by
   [src/dune_rules/opam_create.ml] when synthesising opam files) and a detector
   ([is_dune_default], called by [Resolved_package] during locking to recognise
   opam packages whose build commands match what dune would have emitted). 

   CR-maybe Alizter: shapes whose args are fully determined by the version
   parameters fall out of [default_build_command] automatically (detection
   enumerates its image as [canonical]). Shapes with free args (currently just
   from_3_23's runtest target, handled by [matches_from_3_23]) need a
   hand-written matcher kept in sync with the generator. If more such shapes
   accrue, consider expressing them via a [Conv]-style bidirectional codec (see
   [otherlibs/dune-rpc/conv.mli]) so one description yields both writer and
   reader. With a single free arg, doing it by hand is fine. *)

let dev_var = Package_variable_name.to_opam Package_variable_name.dev
let with_test_var = Package_variable_name.to_opam Package_variable_name.with_test
let with_doc_var = Package_variable_name.to_opam Package_variable_name.with_doc
let pinned_var = OpamVariable.of_string "pinned"
let str s : OpamTypes.arg = OpamTypes.CString s, None

let str_when s v : OpamTypes.arg =
  OpamTypes.CString s, Some (OpamTypes.FIdent ([], v, None))
;;

let cmd args : OpamTypes.command = args, None
let cmd_when v args : OpamTypes.command = args, Some (OpamTypes.FIdent ([], v, None))
let dune = str "dune"
let name : OpamTypes.arg = OpamTypes.CIdent "name", None
let jobs : OpamTypes.arg = OpamTypes.CIdent "jobs", None

let before_1_11 : OpamTypes.command list =
  [ cmd_when pinned_var [ dune; str "subst" ]
  ; cmd [ dune; str "build"; str "-p"; name; str "-j"; jobs ]
  ; cmd_when with_test_var [ dune; str "runtest"; str "-p"; name; str "-j"; jobs ]
  ; cmd_when with_doc_var [ dune; str "build"; str "-p"; name; str "@doc" ]
  ]
;;

let from_1_11_before_2_7 : OpamTypes.command list =
  [ cmd_when pinned_var [ dune; str "subst" ]
  ; cmd
      [ dune
      ; str "build"
      ; str "-p"
      ; name
      ; str "-j"
      ; jobs
      ; str "@install"
      ; str_when "@runtest" with_test_var
      ; str_when "@doc" with_doc_var
      ]
  ]
;;

let from_3_0 ~with_subst ~with_sites : OpamTypes.command list =
  List.concat
    [ (if with_subst then [ cmd_when dev_var [ dune; str "subst" ] ] else [])
    ; [ cmd
          (List.concat
             [ [ dune; str "build"; str "-p"; name; str "-j"; jobs ]
             ; (if with_sites then [ str "--promote-install-files=false" ] else [])
             ; [ str "@install"
               ; str_when "@runtest" with_test_var
               ; str_when "@doc" with_doc_var
               ]
             ])
      ]
    ; (if with_sites
       then
         [ cmd [ dune; str "install"; str "-p"; name; str "--create-install-files"; name ]
         ]
       else [])
    ]
;;

let from_3_23 ~with_subst ~with_sites ~runtest : OpamTypes.command list =
  List.concat
    [ (if with_subst then [ cmd_when dev_var [ dune; str "subst" ] ] else [])
    ; [ cmd
          (List.concat
             [ [ dune; str "build"; str "-p"; name; str "-j"; jobs ]
             ; (if with_sites then [ str "--promote-install-files=false" ] else [])
             ; [ str_when runtest with_test_var; str_when "@doc" with_doc_var ]
             ])
      ]
    ; (if with_sites
       then
         [ cmd [ dune; str "install"; str "-p"; name; str "--create-install-files"; name ]
         ]
       else [])
    ]
;;

(* The dispatcher: returns the shape dune emits for a given set of project
   parameters. Single source of truth — generation uses it directly, and
   detection enumerates its image over the parameter space (see
   [canonical] below). *)
let default_build_command
      ~(dune_version : int * int)
      ~with_subst
      ~with_sites
      ~exclusive_dir
  =
  if dune_version < (1, 11)
  then before_1_11
  else if dune_version < (2, 7)
  then from_1_11_before_2_7
  else if dune_version < (2, 9)
  then from_3_0 ~with_subst:true ~with_sites:false
  else if dune_version < (3, 0)
  then from_3_0 ~with_subst:true ~with_sites:true
  else if dune_version < (3, 23)
  then from_3_0 ~with_subst ~with_sites
  else (
    match exclusive_dir with
    | None -> from_3_0 ~with_subst ~with_sites
    | Some dir -> from_3_23 ~with_subst ~with_sites ~runtest:("@runtest/" ^ dir))
;;

let default_build_command_value ~dune_version ~with_subst ~with_sites ~exclusive_dir =
  default_build_command ~dune_version ~with_subst ~with_sites ~exclusive_dir
  |> OpamPp.print (OpamFormat.V.map_list OpamFormat.V.command)
;;

(* Shapes [default_build_command] emits when [exclusive_dir = None]. The
   from_3_23 family (variable runtest target) is handled by
   [matches_from_3_23]. *)
let canonical : OpamTypes.command list list =
  [ before_1_11
  ; from_1_11_before_2_7
  ; from_3_0 ~with_subst:false ~with_sites:false
  ; from_3_0 ~with_subst:false ~with_sites:true
  ; from_3_0 ~with_subst:true ~with_sites:false
  ; from_3_0 ~with_subst:true ~with_sites:true
  ]
;;

(* The from_3_23 family: pattern-match the build command's last two args
   for the doc and runtest filters, extract the runtest path, and compare
   against the reconstructed canonical. [with_subst] and [with_sites] are
   inferred from the candidate's structure (first cmd is the dev-filtered
   subst step, last cmd is the install-with-create-install-files step) so
   the caller doesn't enumerate the parameter space. *)
let matches_from_3_23 build =
  let subst_dev = cmd_when dev_var [ dune; str "subst" ] in
  let install_with_create =
    cmd [ dune; str "install"; str "-p"; name; str "--create-install-files"; name ]
  in
  let with_subst =
    match build with
    | first :: _ -> Poly.equal first subst_dev
    | [] -> false
  in
  let with_sites =
    match List.rev build with
    | last :: _ -> Poly.equal last install_with_create
    | [] -> false
  in
  let build_index = if with_subst then 1 else 0 in
  match List.nth build build_index with
  | Some (args, None) ->
    (match List.rev args with
     | (OpamTypes.CString "@doc", Some (OpamTypes.FIdent ([], v_doc, None)))
       :: (OpamTypes.CString runtest, Some (OpamTypes.FIdent ([], v_test, None)))
       :: _
       when OpamVariable.equal v_doc with_doc_var
            && OpamVariable.equal v_test with_test_var
            && String.starts_with ~prefix:"@runtest/" runtest ->
       Poly.equal build (from_3_23 ~with_subst ~with_sites ~runtest)
     | _ -> false)
  | _ -> false
;;

(* The "no-targets" family from opam-repository: a bare [dune build -p name
   -j jobs] optionally preceded by a subst step (with [{dev}] or
   [{pinned}]) and optionally followed by a bare runtest. Every variant is
   a subset of [before_1_11]. *)
let no_targets_variants : OpamTypes.command list list =
  let bare_build = cmd [ dune; str "build"; str "-p"; name; str "-j"; jobs ] in
  let bare_runtest =
    cmd_when with_test_var [ dune; str "runtest"; str "-p"; name; str "-j"; jobs ]
  in
  let subst_dev = cmd_when dev_var [ dune; str "subst" ] in
  let subst_pinned = cmd_when pinned_var [ dune; str "subst" ] in
  [ [ bare_build ]
  ; [ subst_dev; bare_build ]
  ; [ subst_pinned; bare_build ]
  ; [ bare_build; bare_runtest ]
  ; [ subst_dev; bare_build; bare_runtest ]
  ; [ subst_pinned; bare_build; bare_runtest ]
  ]
;;

(* Detection: iterate [canonical] for exact-match, plus the families we
   accept beyond what dune itself emits ([no_targets_variants]) and the
   structural family with variable runtest ([matches_from_3_23]).

   Even with a matching [build:] we refuse the classification when the
   opam file carries any of [install], [patches], [substs], or
   [build-env]. When this function returns true the lockfile records
   [(build (dune))], which [src/dune_rules/pkg_rules.ml]
   ([Build_command.Dune]) expands to a bare [dune build -p <name>]: it
   does not run an install step, apply patches, substitute [.in] files,
   or wrap the build in [Withenv] for [build-env]. The non-default
   branch in [src/dune_pkg/lock_pkg.ml] emits those steps explicitly, so
   packages that rely on them must take that path.

   Applying patches at the [source_rules] level instead of inside the
   build action would let us be permissive about [patches:] here, but a
   general solution is non-trivial; for now the classifier stays
   conservative. *)
let is_dune_default (opam_file : OpamFile.OPAM.t) =
  match
    ( OpamFile.OPAM.install opam_file
    , OpamFile.OPAM.patches opam_file
    , OpamFile.OPAM.substs opam_file
    , OpamFile.OPAM.build_env opam_file )
  with
  | [], [], [], [] ->
    let build = OpamFile.OPAM.build opam_file in
    let exact c = Poly.equal c build in
    matches_from_3_23 build
    || List.exists canonical ~f:exact
    || List.exists no_targets_variants ~f:exact
  | _ -> false
;;

let%test_module "matches" =
  (module struct
    (* Survey of ocaml/opam-repository (2026-05) classifying [build:]
       shapes. The "per opam file" column counts every package-version in the
       repository; the "per package" column dedups to the latest version.
       [shape] is the build-shape category alone, ignoring opam fields like
       [install]/[patches]/[substs]/[build-env]; [is_dune_default] accepts
       [default], [no-targets], and [from-3-23] shapes provided those extra
       fields are empty.

                            per opam file     per package (latest)
                            (16,832 dune)       (2,987 dune)
         no-targets             45.78%             44.63%
         extra-step             31.46%             16.37%
         default                15.78%             30.80%
         missing-runtest         3.44%              3.15%
         other                   2.58%              3.45%
         extra-flag              0.53%              1.07%
         missing-doc             0.35%              0.40%
         j-literal               0.05%              0.03%
         missing-install         0.02%              0.10%

       Combined [is_dune_default] coverage:
         per opam file        53.94% (9,080 / 16,832)
         per package latest   66.99% (2,001 /  2,987)

       The conservative field gate (rejecting shape-matching packages whose
       [install]/[patches]/[substs]/[build-env] is non-empty) costs almost
       nothing: 87 of 9,167 shape-matching opam files (0.95%) and 24 of 2,025
       latest-version packages (1.19%) are excluded. The dominant disqualifier
       is [install]; [patches], [substs], and [build-env] each affect
       single-digit numbers of latest packages.

       "extra-step" is the largest non-matching bucket. ~78% of it (per latest)
       is [rm -r vendors] followed by [dune build], used by tezos to clean
       vendored deps before building. The remainder is a long tail of
       [./configure]/[sh]/[mv] steps. None can be reproduced by a bare [dune
       build -p name], so they are correctly excluded. *)

    (* Each test feeds an opam-text fragment through [is_dune_default] and
       prints [match] or [no match]; the [%expect] block pins the expected
       outcome. *)
    let show
          ?(install = "[]")
          ?(patches = "[]")
          ?(substs = "[]")
          ?(build_env = "[]")
          build
      =
      let opam =
        let text =
          Printf.sprintf
            "opam-version: \"2.0\"\n\
             build: %s\n\
             install: %s\n\
             patches: %s\n\
             substs: %s\n\
             build-env: %s\n"
            build
            install
            patches
            substs
            build_env
        in
        OpamFile.OPAM.read_from_string
          ~filename:(OpamFile.make (OpamFilename.raw "<test>"))
          text
      in
      print_endline (if is_dune_default opam then "match" else "no match")
    ;;

    (* Run [is_dune_default] over a list of pre-built command lists; print
       [all match] if every one matches, otherwise [FAIL: N mismatch(es)]
       followed by each failing opam file so a regression is easy to
       identify from the test diff. *)
    let show_all cmds_list =
      let mismatches =
        List.filter cmds_list ~f:(fun cmds ->
          let opam = OpamFile.OPAM.with_build cmds OpamFile.OPAM.empty in
          not (is_dune_default opam))
      in
      match mismatches with
      | [] -> print_endline "all match"
      | _ :: _ ->
        Printf.printf "FAIL: %d mismatch(es)\n" (List.length mismatches);
        List.iter mismatches ~f:(fun cmds ->
          let opam = OpamFile.OPAM.with_build cmds OpamFile.OPAM.empty in
          Printf.printf "---\n%s" (OpamFile.OPAM.write_to_string opam))
    ;;

    (* Sanity check that [show_all] dumps the failing opam when a shape
       fails to match (so a regression to a canonical shape produces a
       readable test diff, not just a count). *)
    let%expect_test "show_all prints failing opam on mismatch" =
      show_all [ [ cmd [ str "echo"; str "hi" ] ] ];
      [%expect
        {|
        FAIL: 1 mismatch(es)
        ---
        opam-version: "2.0"
        build: ["echo" "hi"]
        |}]
    ;;

    (* 1. Canonical shapes: every entry in [canonical] plus the four
       from_3_23 instantiations (whose runtest path varies — we test with
       a representative). *)
    let%expect_test "canonical shapes" =
      show_all
        (canonical
         @ [ from_3_23 ~with_subst:false ~with_sites:false ~runtest:"@runtest/sub"
           ; from_3_23 ~with_subst:true ~with_sites:false ~runtest:"@runtest/sub"
           ; from_3_23 ~with_subst:false ~with_sites:true ~runtest:"@runtest/sub"
           ; from_3_23 ~with_subst:true ~with_sites:true ~runtest:"@runtest/sub"
           ]);
      [%expect {| all match |}]
    ;;

    (* Round-trip property: every shape [default_build_command] can
       produce is accepted by [is_dune_default]. If a future generator branch
       is added but not reflected in [canonical] or [matches_from_3_23], this
       test fails with the offending opam dumped by [show_all]. The boundary
       versions [(0,0); (1,11); (2,7); (2,9); (3,0); (3,23)] cover every branch
       in the dispatcher's [if/else if] chain. *)
    let%expect_test "round-trip: every default_build_command output is detected" =
      let versions = [ 0, 0; 1, 11; 2, 7; 2, 9; 3, 0; 3, 23 ] in
      let bools = [ false; true ] in
      let exclusive_dirs = [ None; Some "sub" ] in
      let shapes =
        List.concat_map versions ~f:(fun dune_version ->
          List.concat_map bools ~f:(fun with_subst ->
            List.concat_map bools ~f:(fun with_sites ->
              List.map exclusive_dirs ~f:(fun exclusive_dir ->
                default_build_command ~dune_version ~with_subst ~with_sites ~exclusive_dir))))
      in
      show_all shapes;
      [%expect {| all match |}]
    ;;

    (* 2. Non-canonical shapes that still match: the "no-targets" family
       from opam-repository — variants of [before_1_11] with a bare [dune
       build] and no @-targets. Dune does not emit these today, but
       they're structurally safe to detect (subset of [before_1_11], no
       extra side effects). *)
    let%expect_test "no-targets variants" =
      show_all no_targets_variants;
      [%expect {| all match |}]
    ;;

    (* 3. Non-canonical shapes that do not match. Each illustrates a
       specific bucket from the survey. *)

    let%expect_test "no-targets:doc-as-separate-command" =
      show
        {|[
          ["dune" "build" "-p" name "-j" jobs]
          ["dune" "build" "@doc" "-p" name "-j" jobs] {with-doc}
        ]|};
      [%expect {| no match |}]
    ;;

    let%expect_test "missing-runtest" =
      show
        {|[
          ["dune" "subst"] {dev}
          ["dune" "build" "-p" name "-j" jobs "@install" "@doc" {with-doc}]
        ]|};
      [%expect {| no match |}]
    ;;

    let%expect_test "extra-step: ./configure" =
      show
        {|[
          ["./configure" "--prefix" prefix]
          ["dune" "build" "-p" name "-j" jobs "@install" "@runtest" {with-test} "@doc" {with-doc}]
        ]|};
      [%expect {| no match |}]
    ;;

    let%expect_test "extra-step: rm -rf vendor" =
      show
        {|[
          ["rm" "-r" "vendors"]
          ["dune" "build" "-p" name "-j" jobs "@install" "@runtest" {with-test} "@doc" {with-doc}]
        ]|};
      [%expect {| no match |}]
    ;;

    let%expect_test "install-section non-empty" =
      show
        ~install:{|[ ["make" "install"] ]|}
        {|[
          ["dune" "subst"] {dev}
          ["dune" "build" "-p" name "-j" jobs "@install" "@runtest" {with-test} "@doc" {with-doc}]
        ]|};
      [%expect {| no match |}]
    ;;

    (* A matching [build:] alone is not enough: the [(build (dune))]
       shortcut in [pkg_rules.ml] runs only [dune build -p <name>], so any
       opam field whose effect lives outside that command disqualifies. *)

    let%expect_test "patches non-empty" =
      show
        ~patches:{|[ "required.patch" ]|}
        {|[
          ["dune" "subst"] {dev}
          ["dune" "build" "-p" name "-j" jobs "@install" "@runtest" {with-test} "@doc" {with-doc}]
        ]|};
      [%expect {| no match |}]
    ;;

    let%expect_test "substs non-empty" =
      show
        ~substs:{|[ "config.ml.in" ]|}
        {|[
          ["dune" "subst"] {dev}
          ["dune" "build" "-p" name "-j" jobs "@install" "@runtest" {with-test} "@doc" {with-doc}]
        ]|};
      [%expect {| no match |}]
    ;;

    let%expect_test "build-env non-empty" =
      show
        ~build_env:{|[ FOO = "bar" ]|}
        {|[
          ["dune" "subst"] {dev}
          ["dune" "build" "-p" name "-j" jobs "@install" "@runtest" {with-test} "@doc" {with-doc}]
        ]|};
      [%expect {| no match |}]
    ;;

    let%expect_test "extra-flag: --release" =
      show
        {|[
          ["dune" "subst"] {dev}
          ["dune" "build" "-p" name "-j" jobs "--release" "@install" "@runtest" {with-test} "@doc" {with-doc}]
        ]|};
      [%expect {| no match |}]
    ;;

    let%expect_test "missing-doc" =
      show
        {|[
          ["dune" "subst"] {dev}
          ["dune" "build" "-p" name "-j" jobs "@install" "@runtest" {with-test}]
        ]|};
      [%expect {| no match |}]
    ;;

    let%expect_test "j-literal: -j 1" =
      show
        {|[
          ["dune" "subst"] {dev}
          ["dune" "build" "-p" name "-j" "1" "@install" "@runtest" {with-test} "@doc" {with-doc}]
        ]|};
      [%expect {| no match |}]
    ;;

    let%expect_test "missing-install" =
      show
        {|[
          ["dune" "subst"] {dev}
          ["dune" "build" "-p" name "-j" jobs "@runtest" {with-test} "@doc" {with-doc}]
        ]|};
      [%expect {| no match |}]
    ;;

    (* Structural edge cases without an empirical bucket. *)

    let%expect_test "@doc before @runtest" =
      show
        {|[
          ["dune" "subst"] {dev}
          ["dune" "build" "-p" name "-j" jobs "@install" "@doc" {with-doc} "@runtest" {with-test}]
        ]|};
      [%expect {| no match |}]
    ;;

    let%expect_test "wrong filter on subst" =
      show
        {|[
          ["dune" "subst"] {with-test}
          ["dune" "build" "-p" name "-j" jobs "@install" "@runtest" {with-test} "@doc" {with-doc}]
        ]|};
      [%expect {| no match |}]
    ;;

    (* Representatives of populated bins in the opam-repository survey
       that aren't otherwise pinned by a hand-crafted edge case. *)

    let%expect_test "other: only @install (e.g. tofn)" =
      show
        {|[
          ["dune" "build" "-p" name "-j" jobs "@install"]
        ]|};
      [%expect {| no match |}]
    ;;

    let%expect_test
        "other: @install/@runtest/@doc split across commands (e.g. printbox-text)"
      =
      show
        {|[
          ["dune" "build" "@install" "-p" name "-j" jobs]
          ["dune" "runtest" "-p" name "-j" jobs] {with-test}
          ["dune" "build" "@doc" "-p" name "-j" jobs] {with-doc}
        ]|};
      [%expect {| no match |}]
    ;;

    let%expect_test "no-targets shape with patches (e.g. camltc, labrys)" =
      show
        ~patches:{|[ "fix-dune.patch" ]|}
        {|[
          ["dune" "build" "-p" name "-j" jobs]
          ["dune" "runtest" "-p" name "-j" jobs] {with-test}
        ]|};
      [%expect {| no match |}]
    ;;

    let%expect_test "bare @runtest in from_3_23-shaped build" =
      show
        {|[
          ["dune" "subst"] {dev}
          ["dune" "build" "-p" name "-j" jobs "@runtest" {with-test} "@doc" {with-doc}]
        ]|};
      [%expect {| no match |}]
    ;;

    let%expect_test "from_3_23-shaped build with wrong filter on @doc" =
      show
        {|[
          ["dune" "subst"] {dev}
          ["dune" "build" "-p" name "-j" jobs "@runtest/sub" {with-test} "@doc" {dev}]
        ]|};
      [%expect {| no match |}]
    ;;

    let%expect_test "from_3_23 with_sites=true but install cmd modified" =
      show
        {|[
          ["dune" "build" "-p" name "-j" jobs "--promote-install-files=false" "@runtest/sub" {with-test} "@doc" {with-doc}]
          ["dune" "install" "-p" name]
        ]|};
      [%expect {| no match |}]
    ;;

    let%expect_test "from_3_23 build with --promote-install-files but no trailing install"
      =
      show
        {|[
          ["dune" "build" "-p" name "-j" jobs "--promote-install-files=false" "@runtest/sub" {with-test} "@doc" {with-doc}]
        ]|};
      [%expect {| no match |}]
    ;;
  end)
;;
