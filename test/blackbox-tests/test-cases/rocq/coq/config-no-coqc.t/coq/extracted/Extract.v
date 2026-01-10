Require Import Common.Foo.
Require Import Coq.Classes.CRelationClasses.

From Coq Require Extraction.

Require ExtrOcamlBasic.
Require ExtrOcamlChar.
Require ExtrOcamlNativeString.
Require ExtrOCamlFloats.
Require ExtrOCamlInt63.

Require Import ExtrOcamlNatBigInt.
Require Import ExtrOcamlZBigInt.

Extraction Language OCaml.
Unset Extraction Optimize.

Extraction Blacklist String List Char Core Monad Bool Vector Format Nat Int Option Base Numbers FMapAVL OrderedType Map.

(* Used by Coq's Real library *)
Extract Constant ClassicalDedekindReals.sig_forall_dec => "fun _ -> assert false".
Extract Constant ClassicalDedekindReals.sig_not_dec => false.  (* Ugh *)

Recursive Extraction Library Foo.
Recursive Extraction Library CRelationClasses.
