# <PFX>-NN — <the defect, stated plainly>

**Severity:** <Critical|High|Medium|Low> · **Depth:** <fast|standard|deep> (<one-clause reason>) ·
**Depends on:** <IDs or none> · **Findings:** <F-ids or none>
<!-- For issues raised mid-campaign, add:  **Raised by:** <ID>, <date>, <how it was confirmed> -->
<!-- If a decision overrides part of this doc, add:  > **Overridden by D<n>** — <one line>. -->

## Why this exists

<The problem, and the evidence: file:line, a command and its output. Short. Link F-ids rather
than repeating them.>

## What to build

<The target state. Numbered steps if order matters (e.g. "Step 1 — doc change, own commit,
before any code"). Name existing helpers to reuse. Say what NOT to do where a tempting wrong fix
exists.>

## Files you may touch

- `<path>`
- `<path>`

<!-- Or a stated rule: "Whatever the compiler points you at, except <path>." -->

If a fix needs a file outside this list, stop and report why.

## Acceptance criteria

- [ ] `<literal command>` → <expected result>
- [ ] Baseline unchanged: `<command>` → <number> (was <number> at `<commit>`)
- [ ] <Behavioural claim> — demonstrated by <a test that fails before and passes after>

## Notes

<Optional: traps, related issues, things the next agent should know.>
