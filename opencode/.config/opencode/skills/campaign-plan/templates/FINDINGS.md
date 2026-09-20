# <slug> — findings catalogue

Evidence base for the <PFX>-* issues. All line references verified against the working tree at
`<commit>` (branch `<branch>`, <clean|dirty>) on <YYYY-MM-DD>. Each finding notes whether it was
**confirmed by running code**, **confirmed by reading source**, or is **inspection-only /
unverified**.

Severity: **C**ritical / **H**igh / **M**edium / **L**ow.

## Baseline

| Measure | Command | Result |
|---|---|---|
| build | `<cmd>` | <result> |
| tests | `<cmd>` | <N passed / M skipped> |
| lint | `<cmd>` | <N warnings> |

## The through-line

<The common root cause, if one exists. Delete this section if not.>

---

## What the original report got right, and what is already fixed

### F0 (—) — <title>. *<verification>*

<Evidence.>

---

## <Area>

### F1 (<C|H|M|L>) — <title>. *<verification>*

<Evidence: `file:line`, quoted excerpt, or command + output. Every instance listed, or the
command that lists them — never "and N others".>

**Consequence:** <what goes wrong, concretely>
**Suggested fix:** <one or two lines; the issue file holds the detail>
