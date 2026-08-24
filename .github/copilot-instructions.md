# Copilot Instructions

For full development guidance, see [AGENTS.md](../AGENTS.md).

## Common Commands

The setup workflow prepares Copilot's development environment. Run repository
commands directly:

```bash
dune build @check          # Quick build (recommended)
dune build @install        # Full build
dune runtest dir/          # Run tests in a directory
dune runtest dir/test.t    # Run a specific cram test
dune fmt                   # Auto-format code (run before committing)
dune promote               # Accept test output changes (ask user first)
make dev                   # Full build (bootstraps if necessary)
```

## Reviewing PRs

Follow the repository review checklist in
[doc/dev/prompts/review-check.md](../doc/dev/prompts/review-check.md).
