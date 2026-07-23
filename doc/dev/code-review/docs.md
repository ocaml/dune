# `docs:` review concerns

When reviewing a `docs:` PR, apply these concerns in addition to those in
`general.md`.

## Concerns specific to `docs:`

- **Factual accuracy** against the code today. Verify mechanism details (paths,
  file names, env vars) before trusting them.

- **Right genre.** Reference vs howto vs explanation — reference docs that read
  as prose belong under `doc/howto/`.

- **Don't document internals.** File layouts, lockdir formats, implementation
  details change. Document the user-facing surface.

- **Flag unstable surface** with disclaimers on experimental commands and flags.

- **Markup.** Code voice (`` `dune tools env` ``); mark optional CLI arguments.

- **Open a tracking ticket** for missing features rather than papering over them
  in prose.
