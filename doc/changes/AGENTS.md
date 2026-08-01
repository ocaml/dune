# Changelog entries

Document every notable user-visible change with a changelog fragment. Do not
edit `CHANGES.md` directly. Add the fragment under the appropriate directory:

- `added/` for a new user-visible feature or behavior
- `changed/` for a change to existing user-visible behavior
- `fixed/` for a bug fix that restores the expected behavior
- `unreleased/` only in special cases; normally use one of the other categories

Skip implementation-only changes. Also skip non-notable changes even when they
are user-visible; routine improvements to existing error messages are an
example.

Name the fragment `<PR-number>.md`. Write a concise bullet from the user's
perspective, keep it within 80 columns, and end it with a parenthesized
reference containing the PR number and author, for example:

```markdown
- Explain the change and its benefit to users (#12345, fixes #12300, @author)
```

The `fixes` reference is optional.
