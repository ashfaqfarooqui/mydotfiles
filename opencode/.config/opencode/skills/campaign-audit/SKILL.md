---
name: campaign-audit
description: Investigate a codebase against a question (a bug report, a conformance claim, "is this sound?") and write an evidence catalogue FINDINGS.md into ~/.claude/plans/<slug>/, each finding tagged by how it was verified. Use when the user asks to audit, review in depth, or find out what is actually wrong before planning fixes — "audit X", "is the reporter right?", "what's broken in the solver?". Optional first step before campaign-plan; skip it for a known task list or a clear bug report — go straight to campaign-plan for those.
---

# campaign-audit

> **OpenCode edition** — part of the campaign skill set; execution via campaign-run dispatches
> opencode agents only.

Output: `~/.claude/plans/<slug>/FINDINGS.md`, in the format defined in
`../campaign-plan/reference/format.md` (read its *Findings* section first),
from the template `../campaign-plan/templates/FINDINGS.md` (paths relative to
this skill's directory).

Arguments (from the user's request): `<slug> <question or area>`

## 1. Scope

Agree with the user: repo path, the question being asked, the areas in scope, and the `<slug>`.
If there is an originating report, keep it — the catalogue answers it point by point.

## 2. Baseline

Record commit, branch, clean/dirty tree, and date. Run build / tests / lint once in the
foreground and record each result **with its command**.

## 3. Investigate

- Read the code the question is about **and the code the product actually runs**. The most
  valuable finding in past audits was "the tested path is not the production path": trace from
  the entry point (`main`, the public API) rather than from the tests.
- Check the originating report's claims one by one, including which are already fixed.
- Where a finding can be demonstrated, demonstrate it — a command and its output beats a
  reading. Tag each finding with its verification level honestly.
- Enumerate. For a pattern ("fabricated defaults"), find every instance with a command and
  list them all; record the command so the count can be re-run.
- Parallel read-only Explore agents per area are fine **if the user asks for parallel work**.
  Cross-check anything they report against source yourself before it goes in the catalogue.

## 4. Write FINDINGS.md

- F-ids in order, grouped by area; F0 section for "what the report got right / already fixed".
- A **through-line** section if the findings share a root cause.
- No "and N others".

## 5. Hand over

Summarise for the user: counts by severity, the through-line, and anything that needs their
decision. Suggest the `campaign-plan` skill as the next step; do not write issues from this skill.
