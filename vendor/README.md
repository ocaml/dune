# Vendor Update Tool

This directory contains an OCaml tool for managing Dune's vendored
dependencies.

## Usage

```bash
# Fetch all packages
dune exec vendor/vendor_updater.exe -- fetch

# Fetch a specific package
dune exec vendor/vendor_updater.exe -- fetch cmdliner

# Generate patches for locally modified packages
dune exec vendor/vendor_updater.exe -- patch

# Check patch integrity
dune exec vendor/vendor_updater.exe -- lint

# Check for outdated packages
dune exec vendor/vendor_updater.exe -- outdated

# List all available packages
dune exec vendor/vendor_updater.exe -- list
```

## Commands

All commands (apart from `list`) take any number of project names or
directories to projects as arguments.

- `outdated` - Check for outdated packages compared to upstream
- `list` - List available packages
- `fetch` - Fetch packages from upstream
- `patch` - Generate patches for packages that have been modified locally
- `lint` - Check patch integrity

## Package Specifications

Each package is declaratively defined in `packages.ml`. See there for examples.

