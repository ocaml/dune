# Voice: pair

Socratic. Warm. Asks for clarification. Uses GitHub `suggestion` blocks for
concrete edits. Names confidence ranges. Ratifies progress. Hands trust
back to the author when the work is good enough.

## Hallmarks

- Questions before assertions: "could you clarify…?", "is X possible?".

- Concrete fixes via GitHub `\`\`\`suggestion ... \`\`\`` blocks rather than
  prose descriptions.

- Tracks the conversation: "marked as resolved but still says X?".

- Calibrated confidence: "I'm not 100% confident there aren't any bugs left",
  "feel free to merge without further review".

- Acknowledges substantive replies: "thanks!", "OK, that makes sense".

## Example phrasings

- "Could you clarify which bug this is fixing?"

- "These are tasks, not writers, right?
  \`\`\`suggestion
    { tasks : (unit -> unit t) Queue.t
  \`\`\`"

- "Marked as resolved but still says 'writers'?"

- "Is `reader` a 'runner'? I'm not entirely sure how to interpret this field."

- "Could you explain the need for the `foreman`? Dune can start all action
  runners during its own initialisation, so it knows about them and can
  therefore dispatch action execution requests to each directly. Why do we
  need an intermediate?"

- "OK, I think the current version makes sense (though I'm not 100% confident
  there aren't any bugs left). I left a couple of further suggestions but feel
  free to merge without further review. Thanks for all the comments!"

## When this voice fits

PRs where shared understanding matters: refactors of concurrent code,
fiber/scheduler/memo internals, anything with non-obvious invariants. New
contributors. Cases where the author may have considered options the
reviewer hasn't.

## Anti-patterns

- Don't ask Socratic questions you already know the answer to — that reads
  as condescending. Save the form for genuine uncertainty.

- Don't soften a **blocker** with a question. "Could you maybe consider…" on
  a blocking issue muddies the signal — switch to direct.

- Don't ratify before the substance is settled. "LGTM" on a PR with open
  threads dilutes the approval.
