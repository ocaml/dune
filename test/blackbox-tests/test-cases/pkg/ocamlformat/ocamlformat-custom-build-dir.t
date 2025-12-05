Checks whether dev-tool locking takes custom build directories correctly into account.

  $ . ./helpers.sh

Set up some ocamlformat that we want to install.

  $ ocamlformat_version="0.26.2"
  $ make_fake_ocamlformat "${ocamlformat_version}"
  $ make_ocamlformat_opam_pkg "${ocamlformat_version}"

  $ cat > .ocamlformat <<EOF
  > version = ${ocamlformat_version}
  > EOF

Override the build directory that we want to build in. We do this by replacing
the build directory in `$dev_tool_lock_dir` with our custom build directory.

  $ default_build_dir="_build"
  $ custom_build_dir="_other_build"
  $ default_dev_tool_lock_dir="${dev_tool_lock_dir}"
  $ dev_tool_lock_dir=$(echo "${dev_tool_lock_dir}" | sed "s/^$default_build_dir/$custom_build_dir/")

Create a configuration with this custom build directory

  $ make_project_with_dev_tool_lockdir
  $ enable_pkg

Make sure we don't have a lock dir

  $ [ -e "${dev_tool_lock_dir}"/lock.dune ] || echo "Lock dir does not exist in custom location"
  Lock dir does not exist in custom location
  $ [ -e "${default_dev_tool_lock_dir}"/lock.dune ] || echo "Lock dir does not exist in default location"
  Lock dir does not exist in default location

Install our fake ocamlformat, making sure to override the build directory.

  $ dune tools install ocamlformat --build-dir="${custom_build_dir}"
  Solution for _other_build/.dev-tools.locks/ocamlformat:
  - ocamlformat.0.26.2

This should've worked and picked up our ocamlformat using the lock dir
configuration from the dune-workspace. But also, we should now have a lock dir
at the right location, in our custom build dir.

  $ [ -e "${dev_tool_lock_dir}"/lock.dune ] && echo "Lock dir created in the correct, custom location"
  Lock dir created in the correct, custom location
  $ [ -e "${default_dev_tool_lock_dir}"/lock.dune ] || echo "Lock dir does not exist in default location"
  Lock dir does not exist in default location
