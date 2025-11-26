Require Import Common.Foo.
Require Import Corelib.Classes.CRelationClasses.

From Corelib Require Extraction.

From Corelib Require ExtrOcamlBasic.
From Stdlib Require ExtrOcamlChar.
From Stdlib Require ExtrOcamlNativeString.
From Stdlib Require ExtrOCamlFloats.
From Stdlib Require ExtrOCamlInt63.

From Stdlib Require Import ExtrOcamlNatBigInt.
From Stdlib Require Import ExtrOcamlZBigInt.

Extraction Language OCaml.
Unset Extraction Optimize.

Extraction Blacklist String List Char Core Monad Bool Vector Format Nat Int Option Base Numbers FMapAVL OrderedType Map.

(* Used by Coq's Real library *)
(* Extract Constant ClassicalDedekindReals.sig_forall_dec => "fun _ -> assert false". *)
(* Extract Constant ClassicalDedekindReals.sig_not_dec => false.  (\* Ugh *\) *)

Recursive Extraction Library Foo.
Recursive Extraction Library CRelationClasses.
