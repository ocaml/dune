Stress test for the SAT solver used to resolve opam dependencies: We encode
sudoku as a set of packages, one for each cell of the grid, such that two cells
on the same row, column, or group, can't have the same version. Selecting a
subset of cells with specific versions as our target then forces the opam
solver to resolve the version of all the cells in the sudoku grid.

  $ export DUNE_TRACE=+sat
  $ mkrepo
  $ add_mock_repo_if_needed
  $ make_dune_project 3.11

There are 81 packages for the cells, called `cell_{x}_{y}` and each have 9
different versions corresponding to the value affected to that cell.  We use
three opam conflict classes to ensure that two neighbouring cells can't have
the same value (version) on the same row, column or group.

  $ digits="1 2 3 4 5 6 7 8 9"

  $ for x in $digits; do
  >   for y in $digits; do
  >     g=$(( 3 * ((x - 1) / 3) + (y - 1) / 3 + 1 ))
  >     for v in $digits; do
  >       { echo 'depends: [ "row'$x'" "col'$y'" ]'
  >         echo 'conflict-class: [ "row'$x'_has_'$v'" "col'$y'_has_'$v'" "grp'$g'_has_'$v'" ]'
  >       } | mkpkg "cell_${x}_${y}" "$v"
  >     done
  >   done
  > done

For example, the cell in the top-left corner holding the number 5 is defined by:

  $ cat "$mock_packages/cell_1_1/cell_1_1.5/opam"
  opam-version: "2.0"
  depends: [ "row1" "col1" ]
  conflict-class: [ "row1_has_5" "col1_has_5" "grp1_has_5" ]

Its three conflict classes ensure that neighbouring cells can't hold the same
version, because only one package can for example be part of the class `grp1_has_5`:

  $ cat "$mock_packages/cell_2_3/cell_2_3.5/opam"
  opam-version: "2.0"
  depends: [ "row2" "col3" ]
  conflict-class: [ "row2_has_5" "col3_has_5" "grp1_has_5" ]

To transitively force the resolution of every cell in the grid, each cell
depends on meta packages for its row and column, which themselves depend on
their cells (it's a cycle which encompass the whole grid):

  $ for x in $digits; do
  >   row=""; col=""
  >   for y in $digits; do
  >     row="$row \"cell_${x}_${y}\""
  >     col="$col \"cell_${y}_${x}\""
  >   done
  >   echo "depends: [$row ]" | mkpkg "row$x" 1
  >   echo "depends: [$col ]" | mkpkg "col$x" 1
  > done

For example, the fifth column requires all of its cells:

  $ cat "$mock_packages/col5/col5.1/opam"
  opam-version: "2.0"
  depends: [ "cell_1_5" "cell_2_5" "cell_3_5" "cell_4_5" "cell_5_5" "cell_6_5" "cell_7_5" "cell_8_5" "cell_9_5" ]

The following sudoku puzzles were randomly selected from an online benchmark
for solvers: https://github.com/t-dillon/tdoku/blob/master/test/test_puzzles

  $ puzzle="........2..8.1.9..5....3.4....1.93...6..3..8...37......4......53.1.7.8..2........"

  $ grid() { sed -E 's/.{9}/&\n/g'; }
  $ echo "$puzzle" | grid
  ........2
  ..8.1.9..
  5....3.4.
  ...1.93..
  .6..3..8.
  ..37.....
  .4......5
  3.1.7.8..
  2........
  
We encode a puzzle as a package which depends on the cells with known values as
their version, then extract the completed sudoku grid from the solution found:

  $ solve() {
  >   local puzzle=$1 i=0 r c d
  >   { echo 'opam-version: "2.0"'
  >     echo 'depends: ['
  >     for x in $digits; do
  >       for y in $digits; do
  >         v=${puzzle:i:1}
  >         i=$(( i + 1 ))
  >         if [ "$v" != "." ]; then
  >           echo '  "cell_'$x'_'$y'" {= "'$v'"}'
  >         fi
  >       done
  >     done
  >     echo ']'
  >   } > sudoku.opam
  >   dune pkg lock 2>&1 \
  >     | grep '^- cell_' | sed 's/.*\.//' | tr -d '\n' \
  >     | grid
  >   dune trace cat 2>/dev/null | jq 'include "dune"; satSolveEvents | .args'
  > }

The expected output is the answer and statistics about the opam solver:

  $ solve "$puzzle"
  639847512
  478512963
  512693748
  724189356
  965234187
  183765294
  847921635
  351476829
  296358471
  {
    "num_variables": 2861,
    "num_clauses": 8885,
    "num_decisions": 543,
    "num_conflicts": 485,
    "num_opam_files": 571
  }

Another:

  $ solve '.....5..3..9....4..81.4.......7.......4..2..68...14.3.......2...4...6..79...5..1.'
  427165893
  539278641
  681349725
  216793458
  394582176
  875614932
  758431269
  143926587
  962857314
  {
    "num_variables": 2861,
    "num_clauses": 8885,
    "num_decisions": 1167,
    "num_conflicts": 1061,
    "num_opam_files": 571
  }

And another:

  $ solve '68.9.5.....3...5.84.21.87.339.72.8.........1..45..69...6.8.4..2..1..2.757...13...'
  687935241
  913247568
  452168793
  396721854
  278459316
  145386927
  569874132
  831692475
  724513689
  {
    "num_variables": 2381,
    "num_clauses": 7769,
    "num_decisions": 0,
    "num_conflicts": 0,
    "num_opam_files": 475
  }

We can also test the solver on an impossible sudoku, which shows that the
solver runs multiple times to produce a user-friendly diagnostic of the error.
(this diagnostic is filtered out below by `grep` because it's both verbose and
unreadable after the attempt by `grid` to pretty print the solution)

  $ solve "12$puzzle" | grep '^\(  \|{\|}\)'
  {
    "num_variables": 2781,
    "num_clauses": 8699,
    "num_decisions": 0,
    "num_conflicts": 1,
    "num_opam_files": 555
  }
  {
    "num_variables": 2781,
    "num_clauses": 8699,
    "num_decisions": 0,
    "num_conflicts": 1,
    "num_opam_files": 0
  }
  {
    "num_variables": 3182,
    "num_clauses": 8699,
    "num_decisions": 371,
    "num_conflicts": 0,
    "num_opam_files": 0
  }

Note that sudoku problems are a worst-case scenario for dependency resolution,
so this test is only useful to check for regressions but is not representative
of real-world performances.

In general, to make the opam solver fast, we need to minimize its stats in
decreasing order of importance:

- The `num_opam_files` as reading opam files is expensive I/O and only grows
| the problem size.

- The `num_conflicts` which corresponds to the wasted work by the SAT solver
| (forced backtracking), and/or the `num_decisions` which is proportional to
| the total amount of work done by the SAT solver.
| It's not clear without benchmarking that decreasing one of those at the
| detriment of the other is better.

- While it's good to keep `num_variables` and `num_clauses` low, they are not
| obviously correlated with the complexity of the problem: it's sometimes
| possible to encode the same SAT problem with less clauses or variables by
| using a smarter encoding, but this can backfire by forcing the SAT solver to
| conflict and backtrack more often (because the smart encoding may hide logical
| deductions from the solver, forcing it to discover conflicts by trial and
| error).
