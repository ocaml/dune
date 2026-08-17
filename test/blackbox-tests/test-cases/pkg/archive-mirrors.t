Package archives can be fetched from an opam repository's archive mirrors.

Set up a local repository whose mirror is relative to the repository root:

  $ mkrepo
  $ cat > mock-opam-repository/repo <<'EOF'
  > opam-version: "2.0"
  > archive-mirrors: "cache"
  > EOF

Put a package archive in the checksum-addressed location used by opam
repository mirrors. The package's primary URL is unavailable, so building it
can only succeed if Dune uses the mirror.

  $ mkdir source
  $ echo "from the mirror" > source/value
  $ tar cf source.tar source
  $ checksum=$(md5sum source.tar | cut -f1 -d' ')
  $ prefix=$(printf '%s' "$checksum" | cut -c1-2)
  $ cache=mock-opam-repository/cache/md5/$prefix
  $ mkdir -p "$cache"
  $ cp source.tar "$cache/$checksum"
  $ touch fake-curls

  $ mkpkg foo <<EOF
  > url {
  >  src: "http://localhost:9000/source.tar"
  >  checksum: "md5=$checksum"
  > }
  > EOF

  $ solve foo
  Solution for dune.lock:
  - foo.0.0.1

The resolved mirror is recorded in the lock directory so builds do not depend
on repository configuration:

  $ print_source foo.0.0.1 | dune_cmd subst 'md5=[0-9a-f]+' 'md5=$HASH'
  (source (fetch (url http://localhost:9000/source.tar) (checksum md5=$HASH) (archive_mirrors file://PWD/mock-opam-repository/cache)))

  $ build_pkg foo

A local (file) primary URL is read directly and mirrors are never consulted.
The mirror object for this package is corrupt, so consulting it would emit a
checksum warning:

  $ mkdir local-source
  $ echo "from the local file" > local-source/value
  $ tar cf local.tar local-source
  $ local_checksum=$(md5sum local.tar | cut -f1 -d' ')
  $ local_prefix=$(printf '%s' "$local_checksum" | cut -c1-2)
  $ local_cache=mock-opam-repository/cache/md5/$local_prefix
  $ mkdir -p "$local_cache"
  $ echo "not the local archive" > "$local_cache/$local_checksum"

  $ mkpkg local-archive <<EOF
  > url {
  >  src: "file://$PWD/local.tar"
  >  checksum: "md5=$local_checksum"
  > }
  > EOF

  $ solve local-archive
  Solution for dune.lock:
  - local-archive.0.0.1

The mirror is still recorded in the lock directory:

  $ print_source local-archive.0.0.1 | dune_cmd subst 'md5=[0-9a-f]+' 'md5=$HASH'
  (source (fetch (url file://PWD/local.tar) (checksum md5=$HASH) (archive_mirrors file://PWD/mock-opam-repository/cache)))

  $ build_pkg local-archive

If an object is absent from a mirror, Dune falls back to the package's primary
URL:

  $ mkdir fallback-source
  $ echo "from the primary URL" > fallback-source/value
  $ tar cf fallback.tar fallback-source
  $ fallback_checksum=$(md5sum fallback.tar | cut -f1 -d' ')
  $ echo fallback.tar > fake-curls

  $ mkpkg fallback <<EOF
  > url {
  >  src: "http://localhost:1/fallback.tar"
  >  checksum: "md5=$fallback_checksum"
  > }
  > EOF

  $ solve fallback
  Solution for dune.lock:
  - fallback.0.0.1
  $ build_pkg fallback
  $ cat already-served
  1

A corrupt mirror object is ignored after checksum verification, and the
primary URL is tried next:

  $ mkdir verified-source
  $ echo "checksum verified" > verified-source/value
  $ tar cf verified.tar verified-source
  $ verified_checksum=$(md5sum verified.tar | cut -f1 -d' ')
  $ verified_prefix=$(printf '%s' "$verified_checksum" | cut -c1-2)
  $ verified_cache=mock-opam-repository/cache/md5/$verified_prefix
  $ mkdir -p "$verified_cache"
  $ echo "not the requested archive" > "$verified_cache/$verified_checksum"
  $ echo verified.tar >> fake-curls

  $ mkpkg verified <<EOF
  > url {
  >  src: "http://localhost:2/verified.tar"
  >  checksum: "md5=$verified_checksum"
  > }
  > EOF

  $ solve verified
  Solution for dune.lock:
  - verified.0.0.1
  $ build_pkg verified 2>&1 | dune_cmd subst "$PWD" PWD | dune_cmd subst 'cache/md5/[0-9a-f]+/[0-9a-f]+' 'cache/md5/$PREFIX/$HASH' | dune_cmd subst 'md5=[0-9a-f]+' 'md5=$HASH'
  Warning: Ignoring archive from mirror
  file://PWD/mock-opam-repository/cache/md5/$PREFIX/$HASH
  because its checksum does not match.
  Expected checksum:
  md5=$HASH
  Actual checksum:
  md5=$HASH
  $ cat already-served
  1
  2

HTTP mirrors use the same checksum-addressed layout and are tried before the
primary URL:

  $ cat > mock-opam-repository/repo <<'EOF'
  > opam-version: "2.0"
  > archive-mirrors: "http://localhost:3"
  > EOF
  $ mkdir http-source
  $ echo "from the HTTP mirror" > http-source/value
  $ tar cf http.tar http-source
  $ http_checksum=$(md5sum http.tar | cut -f1 -d' ')
  $ echo http.tar >> fake-curls
  $ echo http.tar >> fake-curls

  $ mkpkg http-mirror <<EOF
  > url {
  >  src: "http://localhost:4/http.tar"
  >  checksum: "md5=$http_checksum"
  > }
  > EOF

  $ solve http-mirror
  Solution for dune.lock:
  - http-mirror.0.0.1
  $ build_pkg http-mirror
  $ cat already-served
  1
  2
  3

Archive mirrors also apply to extra sources:

  $ cat > mock-opam-repository/repo <<'EOF'
  > opam-version: "2.0"
  > archive-mirrors: "cache"
  > EOF
  $ mkdir extra-main
  $ echo "main source" > extra-main/value
  $ tar cf extra-main.tar extra-main
  $ echo "from the extra-source mirror" > extra.txt
  $ extra_main_checksum=$(md5sum extra-main.tar | cut -f1 -d' ')
  $ extra_checksum=$(md5sum extra.txt | cut -f1 -d' ')
  $ extra_main_prefix=$(printf '%s' "$extra_main_checksum" | cut -c1-2)
  $ extra_prefix=$(printf '%s' "$extra_checksum" | cut -c1-2)
  $ mkdir -p "mock-opam-repository/cache/md5/$extra_main_prefix"
  $ mkdir -p "mock-opam-repository/cache/md5/$extra_prefix"
  $ cp extra-main.tar "mock-opam-repository/cache/md5/$extra_main_prefix/$extra_main_checksum"
  $ cp extra.txt "mock-opam-repository/cache/md5/$extra_prefix/$extra_checksum"

  $ mkpkg with-extra <<EOF
  > build: ["sh" "-c" "grep -q 'from the extra-source mirror' extra.txt"]
  > url {
  >  src: "http://localhost:9001/extra-main.tar"
  >  checksum: "md5=$extra_main_checksum"
  > }
  > extra-source "extra.txt" {
  >  src: "http://localhost:9002/extra.txt"
  >  checksum: "md5=$extra_checksum"
  > }
  > EOF

  $ solve with-extra
  Solution for dune.lock:
  - with-extra.0.0.1
  $ grep -c archive_mirrors dune.lock/with-extra.0.0.1.pkg
  2
  $ build_pkg with-extra

When multiple packages use the same checksum, their repository mirrors are
combined. This matters because the fetch rule itself is shared by checksum:

  $ mkdir shared-source
  $ echo "shared archive" > shared-source/value
  $ tar cf shared.tar shared-source
  $ shared_checksum=$(md5sum shared.tar | cut -f1 -d' ')
  $ echo "not the shared archive" > corrupt-shared
  $ echo corrupt-shared >> fake-curls
  $ echo shared.tar >> fake-curls

  $ mkdir -p repo-a/packages/mirror-a/mirror-a.0.0.1
  $ mkdir -p repo-b/packages/mirror-b/mirror-b.0.0.1
  $ cat > repo-a/repo <<'EOF'
  > opam-version: "2.0"
  > archive-mirrors: "http://localhost:5"
  > EOF
  $ cat > repo-b/repo <<'EOF'
  > opam-version: "2.0"
  > archive-mirrors: "http://localhost:6"
  > EOF
  $ cat > repo-a/packages/mirror-a/mirror-a.0.0.1/opam <<EOF
  > opam-version: "2.0"
  > url {
  >  src: "http://localhost:9003/shared.tar"
  >  checksum: "md5=$shared_checksum"
  > }
  > EOF
  $ cat > repo-b/packages/mirror-b/mirror-b.0.0.1/opam <<EOF
  > opam-version: "2.0"
  > url {
  >  src: "http://localhost:9004/shared.tar"
  >  checksum: "md5=$shared_checksum"
  > }
  > EOF
  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (lock_dir
  >  (repositories repo-a repo-b))
  > (repository
  >  (name repo-a)
  >  (url "file://$PWD/repo-a"))
  > (repository
  >  (name repo-b)
  >  (url "file://$PWD/repo-b"))
  > EOF
  $ cat > dune-project <<'EOF'
  > (lang dune 3.20)
  > (package
  >  (name consumer)
  >  (allow_empty)
  >  (depends mirror-a mirror-b))
  > EOF

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - mirror-a.0.0.1
  - mirror-b.0.0.1
  $ build_pkg mirror-a 2>&1 | dune_cmd subst 'md5/[0-9a-f]+/[0-9a-f]+' 'md5/$PREFIX/$HASH' | dune_cmd subst 'md5=[0-9a-f]+' 'md5=$HASH'
  Warning: Ignoring archive from mirror
  http://localhost:5/md5/$PREFIX/$HASH because its
  checksum does not match.
  Expected checksum:
  md5=$HASH
  Actual checksum:
  md5=$HASH
  $ cat already-served
  1
  2
  3
  5
  6

The repository metadata is also read when the opam repository comes from Git.
Relative mirrors cannot be resolved against a git URL and are skipped, and
mirrors whose scheme dune cannot download from are dropped with a warning at
solve time:

  $ cat > mock-opam-repository/repo <<'EOF'
  > opam-version: "2.0"
  > archive-mirrors: [
  >   "cache"
  >   "https://mirror.example/cache"
  >   "git+ssh://mirror.example/cache"
  > ]
  > EOF
  $ (
  >   cd mock-opam-repository
  >   git init --quiet
  >   git add repo packages
  >   git commit --quiet -m "Repository metadata"
  > )
  $ create_mock_repo "git+file://$PWD/mock-opam-repository"
  $ make_project with-extra > dune-project
  $ dune pkg lock 2>&1 | grep -A1 Warning
  Warning: Ignoring unsupported opam archive mirror
  git+ssh://mirror.example/cache.
Both sources record only the absolute HTTP mirror:

  $ grep -o 'archive_mirrors [^)]*' dune.lock/with-extra.0.0.1.pkg
  archive_mirrors https://mirror.example/cache
  archive_mirrors https://mirror.example/cache
