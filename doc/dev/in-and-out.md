# In-and-out

WIP notes

## Aims

- To enable dogfooding dune pkg in dune
  - Challenge: even partially finished, will be give a lot of benefits to other
 users

## How to proceed:

- Have shon start asking questions.
- Review Puneeth's Commits?

## High-level domain model

The following attempts to present the highest-possible level analysis of the domain and
problem, from a top-down representation of the problem space. (Use of Fstar is
chosen for purely subjective reasons, as any language capable of high-level
specification would suffice.)

```fstar
module InAndOut

type set (a : Type) = | Set : set a

/// Packages are the principle object in the domain of package management.  We
/// index packages by a type that allows differentiating sorts of packages.
type pkg (a:Type) = | Pkg : #sort:a -> pkg a

/// Because dune package management is part of a project management system used
/// for building new packages, packages are indexed by their locality: it
/// matters whether we are reasoning about a package defined in our projects
/// source code, or defined by sources fetched from someone else's computer.
type locale = 
  // Packages defined in the workspace
  | Local
  // Packages from an opam repository or pin
  | Remote
type dune_pkg = pkg locale

let is_local_pkg (p:dune_pkg) = p.sort = Local
let is_remote_pkg (p:dune_pkg) = p.sort = Remote

type local_pkg = p:dune_pkg{is_local_pkg p}
type remote_pkg = p:dune_pkg{is_remote_pkg p}

/// The dependencies required by a package
///
/// The current limitation imposed by the in-and-out out problem is captured in
/// the following refinement placed on the set of dependencies: remote packages
/// are only able to depend on orher remote packages: installing remote packages
/// that depend on local packages is not permitted.
assume
val pkg_dependencies
  : target:dune_pkg 
  -> set (p:dune_pkg{if is_remote_pkg target then remote_pkg else dune_pkg})


/// But, what prevents us from simply lifing this limitation?
/// It is complexity in the domain of build management.

/// "A software build is the process of converting source code files into
/// standalone software artifact(s)"
type src = | Files
type artifact_sort = | Bin | Lib
type artifact = | Artifact : sort:artifact_sort -> artifact

/// Builds also have dependencies, which are artifacts required to build the
/// sources.
type dep (a : Type) = | Dep : #sort:a -> artifact:artifact -> dep a

/// Again, because dune manages projects, the locality of dependencies is
/// important: we have complete transparency with artifacts built from our
/// projects source code, while artifacts obtained elsewhere may be completely
/// opaque.
type dune_dep = dep locale
let is_local (d:dune_dep) = d.sort = Local
let is_remote (d:dune_dep) = d.sort = Remote

type local_dep = d:dune_dep{is_local d}
type remote_dep = d:dune_dep{is_remote d}

/// A build target specifies what to build, and from which prerequisites
type target =
  { 
  /// the name for the resulting artifact
  name : string; 
  /// the kind of artifact to build
  sort : artifact;
  /// the sources to build from
  src : src;
  /// identifiers specifying the required dependencies
  deps : set string
  }
  
/// A `target` gives us everything we need to perform a build, except,
/// critically, for the actual dependencies themselves:
assume
val build 
  : name:string -> sort:artifact_sort -> src:src // Available from the target
  -> deps:(set dune_dep) // These are specified by the target.deps, but must be found
  -> set (a:artifact{a.sort = sort})

/// Therefore, to perform a build, we need a way to obtain the dependency for
/// each dependecy specificaton.
assume
val find_dep 
  : string 
  -> option dune_dep

/// For dependencies defined locally, dune can search the artifacts defined
/// within the local workspace, build any needed, and provide the resulting
/// dependency.
assume
val find_local_dep
  : string
  -> option local_dep

/// But for dependencies from remote packages, there is no way to know which
/// package is provided by a given backage without building it, and dune
/// currently requires building all the remote packages at once.
assume
val find_remote_dep
  : string
  -> (all_remote_pkgs:set remote_pkg)
  -> option remote_dep

/// Some package in `all_remote_pkgs` could depend on the target we are trying
/// to build, so given that we have to build all the remote packages at once, we
/// would be stuck on a build cycle even tho there was no cycle at the package
/// level.

/// But why are we currently constrained to building all remote packages together?
/// It is complexity in the domain of OCaml compilation.
/// TODO
```

##### Qualifications

Difference between public and private components qualifies `defined_in_pkg` in
practice. For this exercise we only care about public.

### Current Architecture

Given `e : exec`, we want to build `e` as a target.

How do derive deps of e, given our rules.
let deps = ?
- to build e
  - we must recursively find compontent set to build each needed target
  - we have thus sequenced this recursively for all opam packages
  - apply restriction: when building opam packages, we restrict where we look for
 components coming from package to only being in lock_dir
    - local_pkg can depend on local_pkg or opam_pkg
    - opam_pkg can depend only on opam_pkgs
      - In-and-out says, why can't we make opam_pkgs depend on local_pkgs
      - We currently forbid this, but if we allow it, we then hit technical issues.
- given a `pkg set`
- current sequencing:
  - deps : pkg set
  - all_components = union components deps
  
- to build `e : exec : component`
- `let build_deps : componenet set = component_deps e`
  - Build deps in workspace, I know how to build easily
  - If they came from a package, then I have to go build the packages, then we
 will recurse
- `let p : local_pkg = defined_in_pkg e` package in which `e` is defined
- `let pkg_deps : pkg set  = pkg_depends p`
- `e` has a build dep defined in a package `A`
- `A` has a package dep defined in the local workspace

- Upshot: we cannot model this problems core issues without going into build
 analysis.
  - The limitation `pkg_depends : opam_pkg -> opam_pkg set` is actually an
 artifact of the fact that we must solve build problems in order to check for
 cycles etc.

- When a want a lib, I don't just ask for lib, I ask for package and then lib in
 it
  - There is no way to search for components without knowing which package they
 came from
  - If we had this up front, then we could just do a meta-data lookup
    - We'd just depend direct deps and build everything in order
  - But problem is we need to build the packages in order do determine the
 components they offer
- Two levels of "layouts"/build plans: course and fine


### Problem

E.g.,

- Working on exec `e`
- `e` depends on libs `ls`
  - defined in stanza
  - in `my_package` via package stanza
- `ls` are in packages
- how does dune know where to find deps?
    - dune uses `findlib` to look for packages in install locations


### Implementation details

- fetch : url -> source
- install : source -> install_layout
- "Cookie" ~ the install layout (data pointing to all dirs from install layout)
  - Name? Treated as a db entry, borrowed from web domain
- Install sections amount to around 6 environment variables we have to tweak
 with dune.
  - We use the same machinery used for "installed libs" (e.g. from opam)
  - Library lookup scope, as if things installed by ocaml lookup path
- We have a difference between a build and an install layout
- With packages there occur in install layout
  - Currently all install rules point at same directory

### Focus

- How can I make local packages more like opam packages

### Walkthru of Puneeth's changes in his branch

- "Super-context": context + e.g., env management, updates to OCAMLPATH.
- 

#### Commit 1: remove restriction on opam_pkg deps

- Allowed generating lock dirs that include workspace depends
- Found 3 cases:
  - 1. Lookup when trying to build and looking up libs
    - Looks in DB with list of pkgs with a "scope"
    - At context level
    - Addressed by narrowing scope to pkg level
    - Allows narrowing down deps we need to look at componenets
  - 2. ?
- ?
There are pkgs that introduce libs that 

#### Commit 2:

- Complication from META file construction
  - They are produced in build then symlinked back into staging area
  - (Install directory)
  - META file described install layout, so needs to be located in the
 materialized info
  - Because we depend on META dirs and they are in build dirs, forces a cycle
 where we depend on all lockdir packages
  - Generating meta files (rather than symlinking) fixes cycle 
- When we do binary lookup, algorithm is:
  - Look in workspace, and if not found, lookup in path
  - But this should have package scoping
  - Not currently possible to determine where binaries are from without building
 everything 
- Two special cases:
  - ocaml packages
  - when looking up deps we can depend on all pkgs
- In general: we cannot determine what packages we need to build in order to get
 a given component
- Fixes for `%{bin:...}` but not `bin_available`

#### Commit 3: 
#### Commit 4:
#### Commit 5:
#### Commit 6:

## Ali's ongiong work

- Focus is install layouts
  - Current bottleneck for parallelizing

### Next up:

- workspace dependencies
- allow intermixing of workspace and lockdir packages
- cycle detection in solver
- 

Not accounted for in TODO:

- PPX
- Tests for melange, roq
- `bin_available` and `lib_available`
- bin and lib variables
- 
