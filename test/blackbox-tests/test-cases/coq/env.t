Test cases for coq.theory and the env stanza to set coq flags, we
test all possibilities [Y/N/I/A] = [Yes/No/Inherited/Absent] :

| dune | dune-workspace |
| A    | A              |
| A    | I              |
| A    | N              |
| A    | Y              |
| I    | A              |
| I    | I              |
| I    | N              |
| I    | Y              |
| N    | A              |
| N    | I              |
| N    | N              |
| N    | Y              |
| Y    | A              |
| Y    | I              |
| Y    | N              |
| Y    | Y              |

Common files

  $ cat > foo.v <<EOF
  > (* This will only compile with -type-in-type *)
  > Definition t := Type.
  > Definition false : t := t.
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 2.8)
  > (using coq 0.3)
  > EOF

Cases for A

  $ cat > dune <<EOF
  > (coq.theory
  >  (name basic))
  > EOF

Case A / A

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > EOF

  $ dune build @all
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

  $ dune build @all --profile unsound
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

Case A / I

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > (env (unsound (coq (flags :standard))))
  > EOF

  $ dune build @all
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

  $ dune build @all --profile unsound
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

Case A / N

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > (env (unsound (coq (flags ))))
  > EOF

  $ dune build @all
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

  $ dune build @all --profile unsound
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

Case A / Y

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > (env (unsound (coq (flags -type-in-type))))
  > EOF

  $ dune build @all
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

  $ dune build @all --profile unsound
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))

Cases for I

  $ cat > dune <<EOF
  > (coq.theory
  >  (name basic))
  > (env (unsound (coq (flags :standard))))
  > EOF

Case I / A

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > EOF

  $ dune build @all
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

  $ dune build @all --profile unsound
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

Case I / I

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > (env (unsound (coq (flags :standard))))
  > EOF

  $ dune build @all
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

  $ dune build @all --profile unsound
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

Case I / N

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > (env (unsound (coq (flags))))
  > EOF

  $ dune build @all
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

  $ dune build @all --profile unsound
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

Case I / Y

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > (env (unsound (coq (flags -type-in-type))))
  > EOF

  $ dune build @all
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

  $ dune build @all --profile unsound
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))

Cases for N

  $ cat > dune <<EOF
  > (coq.theory
  >  (name basic))
  > (env (unsound (coq (flags))))
  > EOF

Case N / A

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > EOF

  $ dune build @all
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

  $ dune build @all --profile unsound
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

Case N / I

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > (env (unsound (coq (flags :standard))))
  > EOF

  $ dune build @all
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

  $ dune build @all --profile unsound
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

Case N / N

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > (env (unsound (coq (flags ))))
  > EOF

  $ dune build @all
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

  $ dune build @all --profile unsound
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

Case N / Y

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > (env (unsound (coq (flags -type-in-type))))
  > EOF

  $ dune build @all
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

  $ dune build @all --profile unsound
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

Cases for Y
  $ cat > dune <<EOF
  > (coq.theory
  >  (name basic))
  > (env (unsound (coq (flags -type-in-type))))
  > EOF

Case Y / A

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > EOF

  $ dune build @all
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

  $ dune build @all --profile unsound
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))

Case Y / I

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > (env (unsound (coq (flags :standard))))
  > EOF

  $ dune build @all
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

  $ dune build @all --profile unsound
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))

Case Y / N

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > (env (unsound (coq (flags))))
  > EOF

  $ dune build @all
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

  $ dune build @all --profile unsound
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))

Case Y / Y

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > (env (unsound (coq (flags -type-in-type))))
  > EOF

  $ dune build @all
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]

  $ dune build @all --profile unsound
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
