# <slug> — master tracker

<What was examined>: `<absolute repo path>` at commit `<sha>` (branch `<branch>`).

**Origin.** <One paragraph: what prompted this campaign.>

Read `FINDINGS.md` for the evidence. Read `AGENT_PROMPT.md` before starting any issue.

> **Path note.** `AGENT_PROMPT.md` lives in this plans directory —
> `<absolute path>/AGENT_PROMPT.md` — **not** in the repo. Always give agents absolute paths.

---

## ▶ START HERE

<!-- ≤ ~40 lines. OVERWRITE this section every session; never append. -->

**Status <YYYY-MM-DD>:** <N of M issues done; branch; pushed or not; tree clean or not.>

**Next, in order:**
1. <ID> — <why it is next>
2. <ID>

**Live hazards:** <anything that will bite the next dispatch>

**Awaiting the user:** <open decisions, pushes, merges — or "nothing">

---

## The through-line

<Common root cause, if any. Delete if none.>

---

## Wave plan

Issues inside a wave touch disjoint files and may run in parallel. Waves run in order.

### Wave 0 — <name> (<serial|parallel>)
| ID | Title | Sev | Files held |
|---|---|---|---|
| [<PFX>-01](<PFX>-01-<slug>.md) | | | |

---

## Status

| ID | Status | Depth | Commit | Outcome |
|---|---|---|---|---|
| <PFX>-01 | ☐ | standard | | · [doc](<PFX>-01-<slug>.md) |

Legend: ☐ ready · ◐ in progress · ☑ done · ⊘ blocked · ✗ won't fix

---

## Decisions

<!-- A decision beats the issue doc it contradicts; add a pointer at the top of that doc. -->

- **D1 / <ID> — <question>.** <Open | Resolved YYYY-MM-DD: answer and reason.>

---

## Standing constraints

<!-- Should trend to empty: move each into AGENT_PROMPT.md once it is permanent. -->

---

## Depth policy

| Issue | Depth | Why |
|---|---|---|

---

## Dispatch hazards

Environment pre-check, <YYYY-MM-DD>:

| Check | Result | Consequence |
|---|---|---|
| Relative path deps | | |
| Other worktrees / sessions | | |
| Commit signing | | |
| Attribution policy | | |
| Hooks | | |
| Gate runtime | | |
| Protected paths | | |

**Safe run modes:** <subagent serial | worktrees | herdr | parallel-no-commit> — <why>

---

## Log

<!-- One dated line per event. Narrative goes in the issue's Outcome appendix. -->

- <YYYY-MM-DD> — campaign planned: <N> issues in <W> waves.
