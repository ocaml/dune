# Rule streaming

This document describes a new design for the production of build rules
in Dune. The new design aims to be more natural, easier to reason
about and to make existing features work well with newer ones such as
directory targets.

It was originally written by Jérémie Dimino as part of the
[streaming RFC](https://github.com/ocaml/dune/pull/5251), and later on moved
into the dev documentation.


## Problem

The [rule production](./rule-production.md) document exposes a concrete problem
with directory targets, but there is also a general sense of messiness in the
way things work. Generating rules for multiple directories at once is
natural, but the current encoding is odd.

## Proposal

The proposal is to add the following rule: `gen_rules ~dir` is allowed
to produced rules in `dir` or any of its descendant only. It is not
allowed to produce rules anywhere else.

`Load_rules.load_dir ~dir` will then always call itself recursively on
the parent of `dir` and take the union of the rules produced by
`gen_rules` for `dir` and the ones produced by the recursive
call. `gen_rules` will no longer have to redirect a call via
`Load_rules.load_dir_and_produce_its_rules`, which we would simply
remove.

This introduces a cycle with all `copy_files` stanza that copy files
from a sub-directory. We propose the break this cycle by introducing
laziness in the rule production code.

### Generating rules with a mask

The idea is that when we produce rules, we will produce rules under
a current active "mask" that tells us where we are allowed to generate
files or directories.  Trying to produce a rule with targets not
matched by this mask will be a runtime error.

When entering `gen_rules ~dir`, the initial mask will be: "any files
and directories that is a descendant of directory `dir`".

We can then narrow the mask to a sub-mask:

```ocaml
val narrow : Target_mask.t -> unit Memo.t -> unit Memo.t
```

With `narrow mask m`, `m` would only be allowed to produce rules whose
target are matched by the intersection of `mask` and the current
mask. `m` wouldn't be evaluated eagerly. Instead, `gen_rules` would
now return a set of direct rules as well as a list of
`(Target_mask.t * unit Memo.t)`. Let's call such a pair a
suspension. A suspension can be forced by evaluation its second
component. Doing so will yield a list of rules matched by the mask and
a new list of suspension.

### Staged rules loading

The next step is to stage `Load_rules.load_dir`. In addition to taking
a directory, `load_dir` will now also take a mask and will return the
set of rules for this mask. To do that, it might need to force a bunch of
suspensions recursively.


### How does that help?

We will put `copy_rules` under a `narrow <only file targets in current
dir>`. In order to determine if a directory is part of a directory
target in an ancestor directory, we wouldn't need to force this
suspension.

### Difficulties

Interpreting a `library` stanza requires knowing the set of `.ml`
files in the current directory. Knowing this requires interpreting
`copy_files` in the current directory. So the interpretation of
`library` stanzas will need to go under a `narrow` as well.
