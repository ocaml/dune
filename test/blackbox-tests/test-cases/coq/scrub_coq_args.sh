#!/bin/sh

# This script is used for scrubbing the output of Dune actions calling Coq
# binaries. It contains some common patterns which are scurbbed for any
# non-reproducible paths and the arguments are displayed line by line.

# \f (form feed) is used as a delayed new line for sed

sed 's/$ (cd .*coqc/coqc/' $1 | # cleanup coqc
sed 's/$ (cd .*coqdep/coqdep/' | # cleanup coqdep
sed 's/$ (cd .*coqdoc/coqdoc/' | # cleanup coqdoc
sed 's/)//' | # remove trailing )
sed 's/ -I/\f-I/g' | # new line for each -I
sed 's/ -nI/\f-nI/g' | # new line for each -nI
sed 's/ -R/\f-R/g' | # new line for each -R
sed 's/ -Q/\f-Q/g' | # new line for each -Q
sed 's/ -w/\f-w/g' | # new line for each -w
sed 's/ -boot/\f-boot/g' | # new line for each -boot
sed 's/ -native-compiler /\f-native-compiler /g' | # new line for each -native-compiler
sed 's/-R [^\s\f]*coq/-R coq/g' | # scrub -R with coq
sed 's/-I [-A-Za-z0-9\/_\.]*lib\//-I lib\//g' | # scrub -I with lib/
sed 's/-nI [-A-Za-z0-9\/_\.]*lib\//-nI lib\//g' | # scrub -nI with lib/
sed 's/\(-R [^\f]* [^\f]*\) \(.*\)/\1\f\2/g' | # new line after each -R
sed 's/\(-I lib\/findlib\)\f\(.*\)/\2\f\1/g' | # move lib/findlib to the end
sed 's/\(-I lib\/zarith\)\f\(.*\)/\2\f\1/g' | # move lib/zarith to the end
tr '\f' '\n' # replace form feed with new line
